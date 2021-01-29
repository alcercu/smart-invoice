import {
  Button,
  Heading,
  Link,
  Text,
  useBreakpointValue,
  VStack,
} from '@chakra-ui/react';
import { BigNumber, utils } from 'ethers';
import React, { useCallback, useContext, useState } from 'react';

import { ReactComponent as LockImage } from '../assets/lock.svg';
import { Web3Context } from '../context/Web3Context';
import { OrderedTextarea } from '../shared/OrderedInput';
import { ADDRESSES } from '../utils/constants';
import { getResolverString, getToken, getTxLink } from '../utils/helpers';
import { lock } from '../utils/invoice';
import { uploadDisputeDetails } from '../utils/ipfs';
import { Loader } from './Loader';

const { ARAGON_COURT, LEX_DAO } = ADDRESSES;

export const LockFunds = ({ invoice, balance }) => {
  const { provider } = useContext(Web3Context);
  const { address, resolver, token } = invoice;
  const resolverString = getResolverString(resolver);
  const { decimals, symbol } = getToken(token);
  const [disputeReason, setDisputeReason] = useState('');
  const fee =
    resolver !== ARAGON_COURT
      ? `${utils.formatUnits(
          BigNumber.from(balance).div(20),
          decimals,
        )} ${symbol}`
      : `150 DAI`;

  const [locking, setLocking] = useState(false);
  const [transaction, setTransaction] = useState();
  const buttonSize = useBreakpointValue({ base: 'md', md: 'lg' });

  const lockFunds = useCallback(async () => {
    if (provider && !locking && balance.gt(0) && disputeReason) {
      setLocking(true);
      const detailsHash = await uploadDisputeDetails({
        reason: disputeReason,
        invoice: address,
        amount: balance.toString(),
      });

      try {
        const tx = await lock(provider, address, detailsHash);
        setTransaction(tx);
        await tx.wait();
        window.location.href = `/invoice/${address}`;
      } catch (lockError) {
        // eslint-disable-next-line
        console.error({ lockError });
      }

      setLocking(false);
    }
  }, [provider, locking, balance, address, disputeReason]);

  if (locking) {
    return (
      <div className="lock-funds">
        <h1> Locking Funds </h1>
        {transaction && (
          <a
            href={getTxLink(transaction.hash)}
            target="_blank"
            rel="noopener noreferrer"
          >
            Follow Transaction on Explorer
          </a>
        )}
        <div className="locking-funds">
          <Loader size="6rem" />
          <LockImage className="image" />
        </div>
      </div>
    );
  }

  return (
    <VStack w="100%" spacing="1rem">
      <Heading
        fontWeight="normal"
        mb="1rem"
        textTransform="uppercase"
        textAlign="center"
      >
        Lock Funds
      </Heading>

      <Text textAlign="center" fontSize="sm" mb="1rem">
        Locking freezes all remaining funds in the contract and initiates a
        dispute.
      </Text>
      <Text w="100%">
        Once a dispute has been initiated, {resolverString} will review your
        case, the project agreement and dispute reasoning before making a
        decision on how to fairly distribute remaining funds.
      </Text>

      <OrderedTextarea
        tooltip="Why do you want to lock these funds?"
        label="Dispute Reason"
        value={disputeReason}
        setValue={setDisputeReason}
      />
      <Text color="red.500" textAlign="center">
        <u>{getResolverString(resolver)}</u> charges a {fee} fee to resolve this
        dispute. This amount will be deducted from the locked fund amount.
      </Text>
      <Button
        onClick={lockFunds}
        colorScheme="red"
        isDisabled={!disputeReason}
        textTransform="uppercase"
        size={buttonSize}
        fontFamily="mono"
        fontWeight="normal"
        w="100%"
      >
        {`Lock ${utils.formatUnits(balance, decimals)} ${symbol}`}
      </Button>
      {[LEX_DAO, ARAGON_COURT].indexOf(resolver) === -1 && (
        <Link
          href={
            resolver === LEX_DAO
              ? 'https://github.com/lexDAO/Arbitration/blob/master/rules/ToU.md#lexdao-resolver'
              : 'https://anj.aragon.org/legal/terms-general.pdf'
          }
          isExternal
          color="red.500"
          textDecor="underline"
        >
          Learn about {getResolverString(resolver)} dispute process & terms
        </Link>
      )}
    </VStack>
  );
};
