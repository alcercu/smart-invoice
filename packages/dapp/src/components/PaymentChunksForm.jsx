import {
  Flex,
  Input,
  InputGroup,
  InputRightElement,
  Text,
  VStack,
} from '@chakra-ui/react';
import { utils } from 'ethers';
import React, { useContext } from 'react';

import { CreateContext } from '../context/CreateContext';
import { getToken } from '../utils/helpers';

export const PaymentChunksForm = () => {
  const {
    paymentToken,
    milestones,
    payments,
    setPayments,
    paymentDue,
  } = useContext(CreateContext);
  const tokenData = getToken(paymentToken);
  const { decimals, symbol } = tokenData;
  return (
    <VStack w="100%" spacing="1rem">
      {Array.from(Array(Number(milestones))).map((_val, index) => {
        return (
          <VStack w="100%" spacing="0.5rem" key={index.toString()}>
            <Flex justify="space-between" w="100%">
              <Text fontWeight="700">Payment #{index + 1}</Text>
              <Flex />
            </Flex>
            <InputGroup>
              <Input
                bg="black"
                type="text"
                color="white"
                border="none"
                onChange={e => {
                  if (!e.target.value || isNaN(Number(e.target.value))) return;
                  const amount = utils.parseEther(e.target.value);
                  const newPayments = payments.slice();
                  newPayments[index] = amount;
                  setPayments(newPayments);
                }}
              />
              <InputRightElement color="white">{symbol}</InputRightElement>
            </InputGroup>
          </VStack>
        );
      })}
      <Text w="100%" textAlign="right" color="grey">
        Total Amount Must Add Up to {utils.formatUnits(paymentDue, decimals)}{' '}
        {symbol}
      </Text>
    </VStack>
  );
};
