import { Suspense } from "react";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import Home from "../pages/home";
import PageNotFound from "../pages/pageNotFound";
import CreateWallet from "../pages/wallet/CreateWallet";
import WalletInfo from "../pages/wallet/WalletInfo";
import { Routes } from "./Routes";

const Router = () => {
  return (
    <BrowserRouter>
      <Suspense fallback={<p>Page is loading...</p>}>
        <Switch>
          <Route path={Routes.HOME} component={Home} exact />
          <Route
            path={`${Routes.WALLET_INFO}:id`}
            component={WalletInfo}
            exact
          />
          <Route path={Routes.WALLET_CREATION} component={CreateWallet} exact />
          <Route path="*" component={PageNotFound} exact />
        </Switch>
      </Suspense>
    </BrowserRouter>
  );
};

export default Router;
