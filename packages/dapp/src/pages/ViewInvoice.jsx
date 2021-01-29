import {
  Button,
  Divider,
  Flex,
  Heading,
  HStack,
  Link,
  Modal,
  ModalCloseButton,
  ModalContent,
  ModalOverlay,
  SimpleGrid,
  Stack,
  Text,
  useBreakpointValue,
  VStack,
  Wrap,
  WrapItem,
} from '@chakra-ui/react';
import { BigNumber, utils } from 'ethers';
import React, { useContext, useEffect, useState } from 'react';
import { Redirect } from 'react-router-dom';

import { DepositFunds } from '../components/DepositFunds';
import { Loader } from '../components/Loader';
import { LockFunds } from '../components/LockFunds';
import { ReleaseFunds } from '../components/ReleaseFunds';
import { ResolveFunds } from '../components/ResolveFunds';
import { Web3Context } from '../context/Web3Context';
import { getInvoice } from '../graphql/getInvoice';
import { Container } from '../shared/Container';
import { balanceOf } from '../utils/erc20';
import {
  getDateString,
  getResolverString,
  getToken,
  getTxLink,
} from '../utils/helpers';

export const ViewInvoice = ({
  match: {
    params: { invoiceId },
  },
}) => {
  const { account, provider } = useContext(Web3Context);
  const [invoice, setInvoice] = useState();
  const [balanceLoading, setBalanceLoading] = useState(true);
  const [balance, setBalance] = useState(BigNumber.from(0));
  const [modal, setModal] = useState(false);
  const [selected, setSelected] = useState(0);

  useEffect(() => {
    if (utils.isAddress(invoiceId)) {
      getInvoice(invoiceId).then(i => setInvoice(i));
    }
  }, [invoiceId]);

  useEffect(() => {
    if (invoice && provider) {
      setBalanceLoading(true);
      balanceOf(provider, invoice.token, invoice.address)
        .then(b => {
          setBalance(b);
          setBalanceLoading(false);
        })
        // eslint-disable-next-line no-console
        .catch(balanceError => console.error({ balanceError }));
    }
  }, [invoice, provider]);

  const leftMinW = useBreakpointValue({ base: '10rem', sm: '20rem' });
  const leftMaxW = useBreakpointValue({ base: '30rem', lg: '20rem' });
  const rightMaxW = useBreakpointValue({ base: '100%', md: '40rem' });
  const buttonSize = useBreakpointValue({ base: 'md', lg: 'lg' });
  const smallScreen = useBreakpointValue({ base: true, sm: false });

  if (!utils.isAddress(invoiceId) || invoice === null) {
    return <Redirect to="/" />;
  }

  if (!invoice || balanceLoading) {
    return (
      <Container overlay>
        <Loader size="80" />
      </Container>
    );
  }

  const {
    projectName,
    projectDescription,
    projectAgreement,
    startDate,
    endDate,
    terminationTime,
    client,
    resolver,
    currentMilestone,
    amounts,
    total,
    token,
    released,
    isLocked,
    deposits,
    releases,
  } = invoice;

  const isClient = account.toLowerCase() === client;
  const isResolver = account.toLowerCase() === resolver;
  const { decimals, symbol } = getToken(token);
  const deposited = BigNumber.from(released).add(balance);
  const due = BigNumber.from(total).sub(deposited);

  const amount = BigNumber.from(amounts[currentMilestone]);
  const isReleasable = currentMilestone < amounts.length && amount.lte(balance);
  const isLockable = !isLocked && balance.gt(0);

  const onLock = () => {
    setSelected(0);
    setModal(true);
  };

  const onDeposit = () => {
    if (deposited.lt(total)) {
      setSelected(1);
      setModal(true);
    }
  };

  const onRelease = async () => {
    if (isReleasable && isClient) {
      setSelected(2);
      setModal(true);
    }
  };

  const onResolve = async () => {
    if (isResolver) {
      setSelected(3);
      setModal(true);
    }
  };

  let sum = BigNumber.from(0);
  return (
    <Container overlay>
      <Stack
        spacing="2rem"
        justify="center"
        align="center"
        direction={{ base: 'column', lg: 'row' }}
        w="100%"
        px="1rem"
        py="8rem"
      >
        <Stack
          spacing="1rem"
          minW={leftMinW}
          w="100%"
          maxW={leftMaxW}
          justify="center"
          align="stretch"
          direction={{ base: 'column', md: 'row', lg: 'column' }}
        >
          <VStack align="stretch" justify="center">
            <Heading fontWeight="normal" fontSize="2xl">
              {projectName}
            </Heading>
            {projectDescription && (
              <Text color="white">{projectDescription}</Text>
            )}
            <Link
              href={projectAgreement}
              isExternal
              textDecor="underline"
              color="white"
            >
              Details of Agreement
            </Link>
          </VStack>
          <VStack fontSize="sm" color="grey" align="stretch" justify="center">
            {startDate && (
              <Wrap>
                <WrapItem>
                  <Text>{'Project Start Date: '}</Text>
                </WrapItem>
                <WrapItem>
                  <Text fontWeight="bold">{getDateString(startDate)}</Text>
                </WrapItem>
              </Wrap>
            )}
            {endDate && (
              <Wrap>
                <WrapItem>
                  <Text>{'Project End Date: '}</Text>
                </WrapItem>
                <WrapItem>
                  <Text fontWeight="bold">{getDateString(endDate)}</Text>
                </WrapItem>
              </Wrap>
            )}
            <Wrap>
              <WrapItem>
                <Text>{'Safety Valve Withdrawal Date: '}</Text>
              </WrapItem>
              <WrapItem>
                <Text fontWeight="bold">{getDateString(terminationTime)}</Text>
              </WrapItem>
            </Wrap>
            <Wrap>
              <WrapItem>
                <Text>{'Arbitration Provider: '}</Text>
              </WrapItem>
              <WrapItem>
                <Text fontWeight="bold">{getResolverString(resolver)}</Text>
              </WrapItem>
            </Wrap>
          </VStack>
        </Stack>
        <VStack
          spacing={{ base: '2rem', lg: '1.5rem' }}
          w="100%"
          align="stretch"
          maxW={rightMaxW}
        >
          <Flex
            bg="background"
            direction="column"
            justify="space-between"
            px={{ base: '1rem', md: '2rem' }}
            py="1.5rem"
            borderRadius="0.5rem"
            w="100%"
            color="white"
          >
            <Flex
              justify="space-between"
              align="center"
              fontWeight="bold"
              mb="1rem"
            >
              <Text>
                {smallScreen ? 'Total Amount' : 'Total Project Amount'}
              </Text>
              <Text>{`${utils.formatUnits(total, decimals)} ${symbol}`}</Text>
            </Flex>
            <VStack
              pl={{ base: '0.5rem', md: '1rem' }}
              align="stretch"
              spacing="0.25rem"
            >
              {amounts.map((amt, index) => {
                let tot = BigNumber.from(0);
                let ind = -1;
                let full = false;
                if (deposits.length > 0) {
                  for (let i = 0; i < deposits.length; i += 1) {
                    tot = tot.add(deposits[i].amount);
                    if (tot.gte(sum)) {
                      ind = i;
                      if (tot.sub(sum).gte(amt)) {
                        full = true;
                        break;
                      }
                    }
                  }
                }
                sum = sum.add(amt);

                return (
                  <Flex
                    key={index.toString()}
                    justify="space-between"
                    align={{ base: 'stretch', sm: 'center' }}
                    direction={{ base: 'column', sm: 'row' }}
                  >
                    <Text>Payment Milestone #{index + 1}</Text>
                    <HStack
                      spacing={{ base: '0.5rem', md: '1rem' }}
                      align="center"
                      justify="flex-end"
                      ml={{ base: '0.5rem', md: '1rem' }}
                    >
                      {index < currentMilestone && releases.length > index && (
                        <Link
                          fontSize="xs"
                          isExternal
                          color="grey"
                          fontStyle="italic"
                          href={getTxLink(releases[index].txHash)}
                        >
                          Released{' '}
                          {new Date(
                            releases[index].timestamp * 1000,
                          ).toLocaleDateString()}
                        </Link>
                      )}
                      {!(index < currentMilestone && releases.length > index) &&
                        ind !== -1 && (
                          <Link
                            fontSize="xs"
                            isExternal
                            color="grey"
                            fontStyle="italic"
                            href={getTxLink(deposits[ind].txHash)}
                          >
                            {full ? '' : 'Partially '}Deposited{' '}
                            {new Date(
                              deposits[ind].timestamp * 1000,
                            ).toLocaleDateString()}
                          </Link>
                        )}
                      <Text
                        textAlign="right"
                        fontWeight="500"
                      >{`${utils.formatUnits(amt, decimals)} ${symbol}`}</Text>
                    </HStack>
                  </Flex>
                );
              })}
            </VStack>
            <Divider
              color="black"
              w={{ base: 'calc(100% + 2rem)', md: 'calc(100% + 4rem)' }}
              ml={{ base: '-1rem', md: '-2rem' }}
              my="1rem"
            />
            <VStack
              pl={{ base: '0.5rem', md: '1rem' }}
              align="stretch"
              spacing="0.25rem"
            >
              <Flex justify="space-between" align="center">
                <Text>{smallScreen ? '' : 'Total '}Deposited</Text>
                <Text fontWeight="500" textAlign="right">{`${utils.formatUnits(
                  deposited,
                  decimals,
                )} ${symbol}`}</Text>
              </Flex>
              <Flex justify="space-between" align="center">
                <Text>{smallScreen ? '' : 'Total '}Released</Text>
                <Text fontWeight="500" textAlign="right">{`${utils.formatUnits(
                  released,
                  decimals,
                )} ${symbol}`}</Text>
              </Flex>
              <Flex justify="space-between" align="center">
                <Text>Remaining{smallScreen ? '' : ' Amount Due'}</Text>
                <Text fontWeight="500" textAlign="right">{`${utils.formatUnits(
                  due,
                  decimals,
                )} ${symbol}`}</Text>
              </Flex>
            </VStack>
            <Divider color="black" w="calc(100% + 4rem)" ml="-2rem" my="1rem" />
            <Flex
              justify="space-between"
              align="center"
              color="red.500"
              fontWeight="bold"
              fontSize="lg"
            >
              <Text>
                {isReleasable &&
                  (smallScreen ? 'Next Release' : 'Next Amount to Release')}
                {!isReleasable &&
                  (smallScreen ? 'Due Today' : 'Total Due Today')}
              </Text>
              <Text>{`${utils.formatUnits(
                isReleasable ? amount : amount.sub(balance),
                decimals,
              )} ${symbol}`}</Text>
            </Flex>
          </Flex>
          <SimpleGrid columns={{ base: 2, sm: 3 }} spacing="1rem" w="100%">
            {isResolver ? (
              <>
                <Flex />
                <Button
                  size={buttonSize}
                  variant="outline"
                  colorScheme="red"
                  fontFamily="mono"
                  textTransform="uppercase"
                  onClick={() => onDeposit()}
                >
                  Deposit
                </Button>
                <Button
                  size={buttonSize}
                  variant="outline"
                  colorScheme="red"
                  fontFamily="mono"
                  textTransform="uppercase"
                  onClick={() => onResolve()}
                >
                  Resolve
                </Button>
              </>
            ) : (
              <>
                {!isReleasable && !isClient && <Flex />}
                {isLockable ? (
                  <Button
                    size={buttonSize}
                    variant="outline"
                    colorScheme="red"
                    fontFamily="mono"
                    textTransform="uppercase"
                    onClick={() => onLock()}
                  >
                    Lock
                  </Button>
                ) : (
                  <Flex />
                )}
                {isReleasable && isClient && (
                  <Button
                    size={buttonSize}
                    variant="outline"
                    colorScheme="red"
                    fontFamily="mono"
                    textTransform="uppercase"
                    onClick={() => onDeposit()}
                  >
                    Deposit
                  </Button>
                )}
                {isReleasable && isClient ? (
                  <Button
                    size={buttonSize}
                    gridArea={{
                      base: '2/1/2/span 2',
                      sm: 'auto/auto/auto/auto',
                    }}
                    colorScheme="red"
                    fontWeight="normal"
                    fontFamily="mono"
                    textTransform="uppercase"
                    onClick={() => onRelease()}
                  >
                    Release
                  </Button>
                ) : (
                  <Button
                    size={buttonSize}
                    gridArea={{
                      base: '2/1/2/span 2',
                      sm: 'auto/auto/auto/auto',
                    }}
                    colorScheme="red"
                    fontWeight="normal"
                    fontFamily="mono"
                    textTransform="uppercase"
                    onClick={() => onDeposit()}
                  >
                    Deposit
                  </Button>
                )}
              </>
            )}
          </SimpleGrid>
        </VStack>
        <Modal isOpen={modal} onClose={() => setModal(false)} isCentered>
          <ModalOverlay>
            <ModalContent
              p="2rem"
              maxW="40rem"
              background="background"
              borderRadius="0.5rem"
              color="white"
            >
              <ModalCloseButton
                _hover={{ bgColor: 'white20' }}
                top="0.5rem"
                right="0.5rem"
              />
              {modal && selected === 0 && (
                <LockFunds
                  invoice={invoice}
                  balance={balance}
                  close={() => setModal(false)}
                />
              )}
              {modal && selected === 1 && (
                <DepositFunds
                  invoice={invoice}
                  deposited={deposited}
                  close={() => setModal(false)}
                />
              )}
              {modal && selected === 2 && (
                <ReleaseFunds
                  invoice={invoice}
                  balance={balance}
                  close={() => setModal(false)}
                />
              )}
              {modal && selected === 3 && (
                <ResolveFunds
                  invoice={invoice}
                  balance={balance}
                  close={() => setModal(false)}
                />
              )}
            </ModalContent>
          </ModalOverlay>
        </Modal>
      </Stack>
    </Container>
  );
};
