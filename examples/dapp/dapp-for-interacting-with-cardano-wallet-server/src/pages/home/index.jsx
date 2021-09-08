import React, { useEffect, useState } from "react";
import Button from "../../components/buttons/Button";
import axios from "../../utils/HttpProvider";
import { useHistory } from "react-router-dom";
import { Routes } from "../../router/Routes";

const Home = () => {
  const [wallets, setWallets] = useState([]);
  const [network, setNetwork] = useState(null);

  const history = useHistory();

  useEffect(() => {
    getAvailableWallets();
    getNetworkInfo();
  }, []);

  const changeRoute = (routeName) => {
    history.push(routeName);
  };

  const getNetworkInfo = async () => {
    try {
      const { data } = await axios.get("/network-info");
      setNetwork(data.data.network);
    } catch (error) {
      console.log(error);
    }
  };

  const getAvailableWallets = async () => {
    try {
      const { data } = await axios.get("/wallets");
      setWallets(data.data.wallets);
    } catch (error) {
      console.log(error);
    }
  };

  const viewWalletInfo = (id) => {
    history.push(`${Routes.WALLET_INFO}${id}`);
  };

  return (
    <>
      <div className="p-12 m-auto w-1/2">
        <h1 className="text-3xl mb-4">Available Actions</h1>
        <div className="flex divide-x-4">
          <Button
            onClick={() => {
              changeRoute(Routes.WALLET_CREATION);
            }}
            name="Import Wallet"
          />
          {/* <Button name="Wallet details" />
          <Button name="UTxO Statistics" />
          <Button name="Get Wallet Public Key" /> */}
        </div>
        <h1 className="text-3xl my-4 mt-8">Available Wallets</h1>
        {wallets.map((wallet) => {
          return (
            <div
              className="flex justify-between w-full items-center py-4"
              key={wallet.id}
            >
              <div>
                <div>{wallet.name}</div>
                <h6>{wallet.id}</h6>
              </div>
              <div>
                <Button
                  onClick={() => {
                    viewWalletInfo(wallet.id);
                  }}
                  name="Wallet details"
                />
              </div>
            </div>
          );
        })}
        {network !== null ? (
          <div className="mt-8 w-full">
            <h1 className="text-2xl">Latest Network Info</h1>
            <div className="mt-4 divide-y divide-gray-500">
              {/* <div className="flex justify-between items-center py-2">
                <div>Node Era</div>
                <div>{network.node_era}</div>
              </div> */}
              <div className="flex justify-between items-center py-2">
                <div className="uppercase">{network.node_tip.height.unit}</div>
                <div>{network.node_tip.height.quantity}</div>
              </div>
              <div className="flex justify-between items-center py-2">
                <div>Absolute slot number</div>
                <div>{network.node_tip.absolute_slot_number}</div>
              </div>
              <div className="flex justify-between items-center py-2">
                <div>Slot number</div>
                <div>{network.node_tip.slot_number}</div>
              </div>
              <div className="flex justify-between items-center py-2">
                <div className="uppercase">Time</div>
                <div>{new Date(network.node_tip.time).toLocaleString()}</div>
              </div>
            </div>
          </div>
        ) : null}
      </div>
    </>
  );
};

export default Home;
