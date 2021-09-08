import axios from "../../utils/HttpProvider";
import React, { useEffect, useState } from "react";
import { useParams } from "react-router-dom";
import round from "lodash/round";
import Button from "../../components/buttons/Button";
import Input from "../../components/form/Input";
import isNil from "lodash/isNil";

const WalletInfo = () => {
  const [wallet, setWallet] = useState(null);
  const [keys, setKeys] = useState(null);
  const [transactions, setTransactions] = useState([]);
  const [values, setValues] = useState({
    notes: undefined,
  });
  const params = useParams();

  useEffect(() => {
    getWalletDetails(params.id);
    getWalletTransactions(params.id);
  }, []);

  const handleTransactionInput = (e) => {
    const { name, value } = e.target;
    setValues({ ...values, [name]: value });
  };

  const getWalletTransactions = async (walletId) => {
    try {
      const { data } = await axios.post("wallet-transactions", { walletId });
      setTransactions(data.data.transactions);
    } catch (error) {
      console.log(error);
    }
  };

  const revealPublicKey = async () => {
    try {
      const walletId = params.id;
      const { data } = await axios.post("wallet-publickey", { walletId });
      setKeys(data.data.publicKey);
    } catch (error) {}
  };

  const getWalletDetails = async (walletId) => {
    try {
      const { data } = await axios.post("wallet-info", { walletId });
      setWallet(data.data.wallets);
    } catch (error) {
      alert(JSON.stringify(error.errorText));
    }
  };

  const cardanoExplorer = (id) => {
    const url = `https://explorer.cardano-testnet.iohkdev.io/en/transaction?id=${id}`;
    window.open(url, "_blank").focus();
  };

  const converLoverlaceToAda = (loverlace) => {
    const amount = round(loverlace / 1000000, 2);
    return `${amount} ₳`;
  };

  const createTransaction = async () => {
    if (isNil(values.ada)) {
      alert("Please enter the amount you want to send");
      return;
    }

    if (isNil(values.receiverAddress)) {
      alert("Please enter the address");
      return;
    }

    if (isNil(values.password)) {
      alert("Please enter the password");
      return;
    }

    try {
      const lovelace = 1000000 * Number(values.ada);
      let notes = undefined;

      if (!isNil(values.notes) && values.notes.length > 0) {
        notes = values.notes;
      }

      const walletId = params.id;

      const payload = { ...values, lovelace, notes, walletId };
      await axios.post("transaction", payload);

      setValues({
        notes: undefined,
      });

      getWalletDetails(walletId);
      getWalletTransactions(walletId);
    } catch (e) {
      alert(JSON.stringify(e.errorText));
    }
  };

  return wallet === null ? (
    <>
      <p>Loading...</p>
    </>
  ) : (
    <>
      <div className="p-12 w-1/2 m-auto">
        <div className="flex divide-x-2 items-end justify-between">
          <div>
            <h1 className="text-3xl">{wallet.name}</h1>
            <h5>{wallet.id}</h5>
          </div>
          <h4 className="uppercase">Status: {wallet.state.status}</h4>
        </div>
        <div className="mt-4">
          <div className="flex mb-12 justify-between items-center">
            <div>
              <h3 className="text-xl">Balance</h3>
            </div>
            <div className="text-right">
              <div>
                <h3 className="text-xl">
                  Reward: {converLoverlaceToAda(wallet.balance.reward.quantity)}
                </h3>
              </div>
              <div>
                <h3 className="text-xl">
                  Available:{" "}
                  {converLoverlaceToAda(wallet.balance.available.quantity)}
                </h3>
              </div>
              <div>
                <h3 className="text-xl">
                  Total: {converLoverlaceToAda(wallet.balance.total.quantity)}
                </h3>
              </div>
            </div>
          </div>
          <div className="mt-8">
            <h3 className="text-2xl">Create transaction</h3>
            <div className="flex w-full">
              <div className="w-1/2 pr-2">
                <Input
                  name="ada"
                  placeholder="Amount in ₳"
                  value={values.ada}
                  onChange={handleTransactionInput}
                  type="number"
                />
              </div>
              <div className="w-1/2 pl-2">
                <Input
                  name="receiverAddress"
                  placeholder="Address of the receiver"
                  value={values.receiverAddress}
                  onChange={handleTransactionInput}
                />
              </div>
            </div>
            <div className="flex w-full mt-4">
              <div className="w-1/2 pr-2">
                <Input
                  name="notes"
                  placeholder="Notes (optional)"
                  value={values.notes}
                  onChange={handleTransactionInput}
                />
              </div>
              <div className="w-1/2 pl-2">
                <Input
                  name="password"
                  placeholder="Password"
                  type="password"
                  value={values.password}
                  onChange={handleTransactionInput}
                />
              </div>
            </div>
            <div className="mt-8 flex divide-x-4">
              <Button name="Create transaction" onClick={createTransaction} />
              <Button name="Wallet Public key" onClick={revealPublicKey} />
            </div>
          </div>
          {keys ? (
            <div className="my-4 p-4 bg-yellow-50 border border-yellow-300">
              Public key: {keys}
            </div>
          ) : null}
          <div className="mt-12">
            <h4 className="text-xl">Transactions</h4>
            {transactions.map((transaction, idx) => {
              return (
                <div className="p-4 border border-blue-300 mb-2" key={idx}>
                  <h6 className="text-right mb-4 text-xs">
                    Direction: {transaction.direction}
                  </h6>
                  <h3
                    className="text-xl cursor-pointer text-blue-500"
                    onClick={() => {
                      cardanoExplorer(transaction.id);
                    }}
                  >
                    ID: {transaction.id}
                  </h3>
                  {transaction.inserted_at ? (
                    <h4 className="my-4">
                      Time: {transaction.inserted_at?.time}
                    </h4>
                  ) : null}
                  <h3 className="uppercase">Status: {transaction.status}</h3>
                  <div className="py-4">
                    <div>Receiver</div>
                    <div className="mt-4 break-words">
                      {transaction.inputs[0].address}
                    </div>
                    <div className="mt-2">
                      Index: {transaction.inputs[0].id}
                    </div>
                    <div className="mt-2">
                      Amount:{" "}
                      {converLoverlaceToAda(
                        transaction.outputs[0].amount.quantity
                      )}
                    </div>
                    <div className="mt-2">
                      Fee: {converLoverlaceToAda(transaction.fee.quantity)}
                    </div>
                  </div>
                  {transaction.metadata ? (
                    <div>
                      <h6>Metadata</h6>
                      <div>{JSON.stringify(transaction.metadata)}</div>
                    </div>
                  ) : null}
                </div>
              );
            })}
          </div>
        </div>
      </div>
    </>
  );
};

export default WalletInfo;
