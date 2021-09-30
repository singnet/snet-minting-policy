import instance from "axios";

const API_HOST = process.env.CARDANO_WALLET_HOST;

const axios = instance.create({
  baseURL: API_HOST,
  timeout: 30000,
});

axios.interceptors.response.use(
  (response) => {
    return response;
  },
  (error) => {
    let statusCode = 400;
    let errorText = "Please check your internet connection";

    if (error.response) {
      statusCode = error.response.status || 400;
      try {
        console.log(error.response.data);
        errorText = error.response.data.message || "Bad request, Try later.";
      } catch (e) {
        errorText = "Something went wrong! Try later";
      }
    }
    return Promise.reject({ errorText, statusCode });
  }
);

export default axios;
