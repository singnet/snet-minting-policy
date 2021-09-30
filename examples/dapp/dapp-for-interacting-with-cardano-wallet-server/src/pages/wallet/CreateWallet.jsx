import React, { useState } from "react";
import { useHistory } from "react-router-dom";
import axios from "../../utils/HttpProvider";
import Button from "../../components/buttons/Button";
import Input from "../../components/form/Input";
import { Routes } from "../../router/Routes";

const CreateWallet = () => {
  const [phrase, setPhrase] = useState("");
  const [phrases, setPhrases] = useState([]);
  const [walletName, setwalletName] = useState("");
  const [password, setPassword] = useState("");

  const history = useHistory();

  const addPhrase = () => {
    if (phrase.length < 1) {
      alert("Please enter a phrase");
    } else {
      setPhrases([...phrases, phrase]);
      setPhrase("");
    }
  };

  const createWallet = async () => {
    if (walletName.length < 1) {
      alert("Please enter a wallet name");
    } else {
      try {
        const payload = {
          name: walletName,
          mnemonic_sentence: phrases,
          passphrase: "P@ssw0rd1234!234",
          address_pool_gap: 20,
        };

        await axios.post("/wallets", payload);
        history.push(Routes.HOME);
      } catch (e) {
        alert(JSON.stringify(e.errorText.errorText));
      }
    }
  };

  return (
    <>
      <div className="h-screen">
        <div className="p-12 m-auto w-1/2">
          <h1 className="text-3xl mb-4">Import Wallet from Recovery Phrase</h1>

          <Input
            name="Recovery phase"
            placeholder={`Enter your recovery phrase`}
            value={phrase}
            onChange={(e) => {
              setPhrase(e.target.value);
            }}
          />
          <div className="mt-4">
            <Button name="Add Phrase" onClick={addPhrase} />
          </div>

          <div className="flex w-full divide-x-4 my-4">
            {phrases.map((word, index) => {
              return (
                <div
                  key={index}
                  className="flex flex-col p-2 bg-gray-500 text-white"
                >
                  {word}
                </div>
              );
            })}
          </div>
          {phrases.length > 14 ? (
            <div className="w-full">
              <Input
                name="Wallet name"
                placeholder={`Enter wallet name`}
                value={walletName}
                onChange={(e) => {
                  setwalletName(e.target.value);
                }}
              />
              <div className="my-4">
                <Input
                  type="password"
                  name="Wallet Password"
                  placeholder={`Enter a password`}
                  value={password}
                  onChange={(e) => {
                    setPassword(e.target.value);
                  }}
                />
              </div>
              <Button name="Create Wallet" onClick={createWallet} />
            </div>
          ) : null}
        </div>
      </div>
    </>
  );
};

export default CreateWallet;
