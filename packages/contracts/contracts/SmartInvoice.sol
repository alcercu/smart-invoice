// SPDX-License-Identifier: MIT
// solhint-disable not-rely-on-time, max-states-count

pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Context.sol";
import "@openzeppelin/contracts/proxy/utils/Initializable.sol";
import "./interfaces/ISmartInvoice.sol";
import "./interfaces/IArbitrable.sol";
import "./interfaces/IArbitrator.sol";
import "./interfaces/IWRAPPED.sol";

// splittable digital deal lockers
// w/ embedded arbitration tailored for guild work
contract SmartInvoice is
    ISmartInvoice,
    IArbitrable,
    Initializable,
    Context,
    ReentrancyGuard
{
    using SafeERC20 for IERC20;

    uint256 public constant NUM_RULING_OPTIONS = 5;
    // excludes options 0, 1 and 2
    // Note that Aragon Court treats the possible outcomes as arbitrary
    // numbers, leaving the Arbitrable (us) to define how to understand them.
    // Some outcomes [0, 1, and 2] are reserved by Aragon Court: "missing",
    // "leaked", and "refused", respectively.
    // Note that Aragon Court emits the LOWEST outcome in the event of a tie.

    // solhint-disable-next-line var-name-mixedcase
    uint8[2][6] public RULINGS = [
        [1, 1], // 0 = refused to arbitrate
        [1, 0], // 1 = 100% to client
        [3, 1], // 2 = 75% to client
        [1, 1], // 3 = 50% to client
        [1, 3], // 4 = 25% to client
        [0, 1]  // 5 = 0% to client
    ];

    // 2-year limit on locker
    uint256 public constant MAX_TERMINATION_TIME = 63113904;
    uint256 public MULTIPLIER = 10000;
    uint256 public DIVISOR = 10000;

    address public wrappedNativeToken;

    enum ADR {INDIVIDUAL, ARBITRATOR}
    enum PARTY {NONE, CLIENT, PROVIDER}

    struct Round {
        uint[2] paidFees;
        PARTY sideFunded;
        uint feeRewards;
        mapping(address => uint[3]) contributions;
        uint8 ruling;
    }

    address public client;
    address public provider;
    ADR public resolverType;
    address public resolver;
    address public token;
    uint256 public terminationTime;
    uint256 public resolutionRate;
    bytes32 public details;

    // milestones split into amounts
    uint256[] public amounts;
    uint256 public total = 0;
    // current milestone - starts from 0 to amounts.length
    uint256 public milestone = 0;
    uint256 public released = 0;
    // The contract will be in the locked state if a dispute arises
    bool public locked;
    uint256 public disputeId;
    uint16 public currentRound;
    mapping(uint => Round) public rounds;

    event Register(
        address indexed client,
        address indexed provider,
        uint256[] amounts
    );
    event Deposit(address indexed sender, uint256 amount);
    event Release(uint256 milestone, uint256 amount);
    event Withdraw(uint256 balance);
    event Lock(address indexed sender, bytes32 details);
    event Resolve(
        address indexed resolver,
        uint256 clientAward,
        uint256 providerAward,
        uint256 resolutionFee,
        bytes32 details
    );
    event Rule(
        address indexed resolver,
        uint256 clientAward,
        uint256 providerAward,
        uint256 ruling
    );
    event AppealContribution(PARTY _side, address sender, uint contribution);
    event HasPaidAppealFee(uint disputeId, uint round);

    // solhint-disable-next-line no-empty-blocks
    function initLock() external initializer {}

    function init(
        address _client,
        address _provider,
        uint8 _resolverType,
        address _resolver,
        address _token,
        uint256[] calldata _amounts,
        uint256 _terminationTime, // termination date in seconds since epoch
        uint256 _resolutionRate,
        bytes32 _details,
        address _wrappedNativeToken
    ) external override initializer {
        require(_client != address(0), "invalid client");
        require(_provider != address(0), "invalid provider");
        require(_resolverType <= uint8(ADR.ARBITRATOR), "invalid resolverType");
        require(_resolver != address(0), "invalid resolver");
        require(_token != address(0), "invalid token");
        require(_terminationTime > block.timestamp, "duration ended");
        require(
            _terminationTime <= block.timestamp + MAX_TERMINATION_TIME,
            "duration too long"
        );
        require(_resolutionRate > 0, "invalid resolutionRate");
        require(
            _wrappedNativeToken != address(0),
            "invalid wrappedNativeToken"
        );

        client = _client;
        provider = _provider;
        resolverType = ADR(_resolverType);
        resolver = _resolver;
        token = _token;
        amounts = _amounts;
        for (uint256 i = 0; i < amounts.length; i++) {
            total = total + amounts[i];
        }
        terminationTime = _terminationTime;
        resolutionRate = _resolutionRate;
        details = _details;
        wrappedNativeToken = _wrappedNativeToken;

        emit Register(_client, _provider, amounts);
    }

    function _release() internal {
        // client transfers locker milestone funds to provider

        require(!locked, "locked");
        require(_msgSender() == client, "!client");

        uint256 currentMilestone = milestone;
        uint256 balance = IERC20(token).balanceOf(address(this));

        if (currentMilestone < amounts.length) {
            uint256 amount = amounts[currentMilestone];
            if (currentMilestone == amounts.length - 1 && amount < balance) {
                amount = balance;
            }
            require(balance >= amount, "insufficient balance");

            milestone = milestone + 1;
            IERC20(token).safeTransfer(provider, amount);
            released = released + amount;
            emit Release(currentMilestone, amount);
        } else {
            require(balance > 0, "balance is 0");

            IERC20(token).safeTransfer(provider, balance);
            released = released + balance;
            emit Release(currentMilestone, balance);
        }
    }

    function release() external override nonReentrant {
        return _release();
    }

    function release(uint256 _milestone) external override nonReentrant {
        // client transfers locker funds upto certain milestone to provider
        require(!locked, "locked");
        require(_msgSender() == client, "!client");
        require(_milestone >= milestone, "milestone passed");
        require(_milestone < amounts.length, "invalid milestone");
        uint256 balance = IERC20(token).balanceOf(address(this));
        uint256 amount = 0;
        for (uint256 j = milestone; j <= _milestone; j++) {
            if (j == amounts.length - 1 && amount + amounts[j] < balance) {
                emit Release(j, balance - amount);
                amount = balance;
            } else {
                emit Release(j, amounts[j]);
                amount = amount + amounts[j];
            }
        }
        require(balance >= amount, "insufficient balance");

        IERC20(token).safeTransfer(provider, amount);
        released = released + amount;
        milestone = _milestone + 1;
    }

    // release non-invoice tokens
    function releaseTokens(address _token) external override nonReentrant {
        if (_token == token) {
            _release();
        } else {
            require(_msgSender() == client, "!client");
            uint256 balance = IERC20(_token).balanceOf(address(this));
            IERC20(_token).safeTransfer(provider, balance);
        }
    }

    function _withdraw() internal {
        require(!locked, "locked");
        require(block.timestamp > terminationTime, "!terminated");
        uint256 balance = IERC20(token).balanceOf(address(this));
        require(balance > 0, "balance is 0");

        IERC20(token).safeTransfer(client, balance);
        milestone = amounts.length;

        emit Withdraw(balance);
    }

    // withdraw locker remainder to client if termination time passes & no lock
    function withdraw() external override nonReentrant {
        return _withdraw();
    }

    // withdraw non-invoice tokens
    function withdrawTokens(address _token) external override nonReentrant {
        if (_token == token) {
            _withdraw();
        } else {
            require(block.timestamp > terminationTime, "!terminated");
            uint256 balance = IERC20(_token).balanceOf(address(this));
            require(balance > 0, "balance is 0");

            IERC20(_token).safeTransfer(client, balance);
        }
    }

    // client or main (0) provider can lock remainder for resolution during
    // locker period / update request details
    function lock(bytes32 _details) external payable override nonReentrant {
        require(!locked, "locked");
        uint256 balance = IERC20(token).balanceOf(address(this));
        require(balance > 0, "balance is 0");
        require(block.timestamp < terminationTime, "terminated");
        require(_msgSender() == client || _msgSender() == provider, "!PARTY");

        if (resolverType == ADR.ARBITRATOR) {
            disputeId = IArbitrator(resolver).createDispute{value: msg.value}(
                NUM_RULING_OPTIONS,
                abi.encodePacked(details)
            );
        }
        locked = true;

        emit Lock(_msgSender(), _details);
    }

    function resolve (
        uint256 _clientAward,
        uint256 _providerAward,
        bytes32 _details
    ) external override nonReentrant {
        // called by individual
        require(resolverType == ADR.INDIVIDUAL, "!individual resolver");
        require(locked, "!locked");
        uint256 balance = IERC20(token).balanceOf(address(this));
        require(balance > 0, "balance is 0");
        require(_msgSender() == resolver, "!resolver");

        // calculates dispute resolution fee (div(20) = 5% of remainder)
        uint256 resolutionFee = balance / resolutionRate;

        require(
            _clientAward + _providerAward == balance - resolutionFee,
            "resolution != remainder"
        );

        if (_providerAward > 0) {
            IERC20(token).safeTransfer(provider, _providerAward);
        }
        if (_clientAward > 0) {
            IERC20(token).safeTransfer(client, _clientAward);
        }
        if (resolutionFee > 0) {
            IERC20(token).safeTransfer(resolver, resolutionFee);
        }

        milestone = amounts.length;
        locked = false;

        emit Resolve(
            _msgSender(),
            _clientAward,
            _providerAward,
            resolutionFee,
            _details
        );
    }

    /** @dev Take up to the total amount required to fund a side of an appeal,
      *     reimburse the rest. Create an appeal if both sides are fully funded.
      * @param _side Which side is the appeal for, 0 client, 1 provider.
      */
    function appeal(uint8 _side) external payable {
        require(resolverType == ADR.ARBITRATOR, "!arbitrator");
        require(locked, "!locked");
        require(_side < 2);
        (uint appealPeriodStart, uint appealPeriodEnd) = IArbitrator(resolver)
            .appealPeriod(disputeId);
        require(
            block.timestamp >= appealPeriodStart,
            "Appeal period hasn't started"
        );
        require(block.timestamp < appealPeriodEnd, "Appeal period is over");
        uint appealCost = IArbitrator(resolver).appealCost(
            disputeId, abi.encodePacked(details)
        );
        uint totalCost = appealCost * MULTIPLIER / DIVISOR;

        Round storage round = rounds[currentRound];
        uint contribution = contribute(
            round, PARTY(_side), payable(msg.sender), msg.value, totalCost
        );
        emit AppealContribution(PARTY(_side), msg.sender, contribution);

        if (round.paidFees[uint(_side)] >= totalCost) {
            if (round.sideFunded == PARTY.NONE) {
                round.sideFunded = PARTY(_side);
           } else {
                IArbitrator(resolver).appeal{value: appealCost}(
                    disputeId, abi.encodePacked(details)
                );
                currentRound++;
                round.feeRewards = round.feeRewards - appealCost;
                round.sideFunded = PARTY.NONE;
            }
            emit HasPaidAppealFee(disputeId, currentRound - 1);
        }
    }

    /** Make a contribution.
      * @param _round Round to contribute to.
      * @param _side Side to contribute to.
      * @param _contributor Contributor.
      * @param _amount Amount the contributor is willing to contribute.
      * @param _totalRequired Amount needed for this side.
      * @return The amount of fees contributed by the contributor.
      */
    function contribute(
        Round storage _round,
        PARTY _side,
        address payable _contributor,
        uint _amount,
        uint _totalRequired
    ) internal returns (uint) {
        uint contribution;
        uint remainingETH;
        (contribution, remainingETH) = calculateContribution(
            _amount, _totalRequired - _round.paidFees[uint(_side)]
        );

        _round.contributions[_contributor][uint(_side)] += contribution;
        _round.paidFees[uint(_side)] += contribution;
        _round.feeRewards += contribution;

        if (remainingETH != 0)
            _contributor.send(remainingETH);

        return contribution;
    }

    /** @dev Return the contribution value and reminder from certain amount
      *     and required amount.
      * @param _amount Available amount of ETH for the contribution.
      * @param _requiredAmount Amount of ETH required.
      * @return taken Amount of ETH taken from the initial amount.
      * @return remainder Amount of ETH left after the operation.
      */
    function calculateContribution(uint _amount, uint _requiredAmount)
        internal
        pure
        returns(uint taken, uint remainder)
    {
        if (_requiredAmount > _amount)
            return (_amount, 0);

        remainder = _amount - _requiredAmount;
        return (_requiredAmount, remainder);
    }

    function rule(uint256 _disputeId, uint256 _ruling)
        external
        override
        nonReentrant
    {
        // called by arbitrator
        require(resolverType == ADR.ARBITRATOR, "!arbitrator resolver");
        require(locked, "!locked");
        require(_msgSender() == resolver, "!resolver");
        require(_disputeId == disputeId, "incorrect disputeId");
        require(_ruling <= NUM_RULING_OPTIONS, "invalid ruling");
        uint256 balance = IERC20(token).balanceOf(address(this));
        require(balance > 0, "balance is 0");

        uint8[2] memory ruling = RULINGS[_ruling];
        uint8 clientShare = ruling[0];
        uint8 providerShare = ruling[1];
        uint8 denom = clientShare + providerShare;
        uint256 providerAward = (balance * providerShare) / denom;
        uint256 clientAward = balance - providerAward;

        if (providerAward > 0) {
            IERC20(token).safeTransfer(provider, providerAward);
        }
        if (clientAward > 0) {
            IERC20(token).safeTransfer(client, clientAward);
        }

        milestone = amounts.length;
        locked = false;

        emit Rule(resolver, clientAward, providerAward, _ruling);
        emit Ruling(resolver, _disputeId, _ruling);
    }

    /** @dev Send rewards or reimburse the fees if no dispute was raised.
      * @param _beneficiary Address that made contributions to a request.
      * @param _round Round from which to withdraw.
      */
    function withdrawFeesAndRewards(
        address payable _beneficiary, uint _round
    ) public {
        require(locked, "!");
        Round storage round = rounds[_round];
    }

    // receive eth transfers
    receive() external payable {
        require(!locked, "locked");
        require(token == wrappedNativeToken, "!wrappedNativeToken");
        IWRAPPED(wrappedNativeToken).deposit{value: msg.value}();
        emit Deposit(_msgSender(), msg.value);
    }
}
