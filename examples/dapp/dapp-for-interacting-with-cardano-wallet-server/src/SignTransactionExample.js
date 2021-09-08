import { useEffect, useState } from "react";
import { BlockFrostAPI } from "@blockfrost/blockfrost-js";

import "./App.css";

const cardanoAddresses = require("cardano-addresses");

const nacl = require("tweetnacl");
const util = require("tweetnacl-util");

const App = () => {
  const [message, setMessage] = useState("");
  const [isValidSignature, setSignatureValidity] = useState(false);
  const [keys, setKeys] = useState({
    privateKey: undefined,
    publicKey: undefined,
  });

  const [signature, setSignature] = useState("");

  useEffect(() => {
    getCardanoStatus();
  }, []);

  const getCardanoStatus = async () => {
    const addr =
      "addr1gqtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevuph3wczvf2dwyx5u";

    const inspect = await cardanoAddresses.inspectAddress(addr);
    console.log(inspect);
  };

  const getCardanoStatuses = async () => {
    const API = new BlockFrostAPI({
      projectId: "YOUR API KEY HERE", // see: https://blockfrost.io
    });

    try {
      const latestBlock = await API.blocksLatest();
      const latestEpoch = await API.epochsLatest();
      const health = await API.health();
      const address = await API.addresses(
        "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
      );

      console.log("address", address);
      console.log("latestEpoch", latestEpoch);
      console.log("latestBlock", latestBlock);
      console.log("health", health);
    } catch (err) {
      console.log("error", err);
    }
  };

  const generateAddress = async () => {
    // TODO: Connect to dapp connector
    const keyPair = nacl.sign.keyPair();
    setKeys({ privateKey: keyPair.secretKey, publicKey: keyPair.publicKey });
  };

  const signMessage = async (e) => {
    e.preventDefault();

    if (message.length < 1) {
      alert("Enter message");
      return;
    }

    const msgInUint8Array = util.decodeUTF8(message);
    const signedMessage = nacl.sign(msgInUint8Array, keys.privateKey);
    setSignature(signedMessage);
  };

  const validateSignature = () => {
    const verifiedMsg = nacl.sign.open(signature, keys.publicKey);
    const verifiedMsgInString = util.encodeUTF8(verifiedMsg);

    if (verifiedMsgInString === message) {
      setSignatureValidity(true);
    } else {
      setSignatureValidity(false);
    }
  };

  return (
    <div className="App">
      <header className="App-header">
        <form style={{ marginBottom: 20 }}>
          <input
            type="text"
            onChange={(e) => {
              setMessage(e.target.value);
            }}
            style={{ padding: 10 }}
            value={message}
            placeholder="Message for signing"
          />
        </form>
        <button style={{ marginBottom: 20 }} onClick={signMessage}>
          Sign message
        </button>
        <button onClick={validateSignature}>Validate Signature</button>
        <h2>Valid Signature: {JSON.stringify(isValidSignature)}</h2>
      </header>
    </div>
  );
};

export default App;
