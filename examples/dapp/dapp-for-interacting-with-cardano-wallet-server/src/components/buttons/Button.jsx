import React from "react";

const Button = ({ name, onClick }) => {
  return (
    <>
      <button
        className="bg-black p-4 text-white focus:outline-none"
        onClick={onClick}
      >
        {name}
      </button>
    </>
  );
};

export default Button;
