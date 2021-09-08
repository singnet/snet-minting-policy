import React from "react";

const Input = ({ name, value, onChange, placeholder, type = "text" }) => {
  return (
    <>
      <input
        className="bg-gray-300 p-4 w-full"
        type={type}
        name={name}
        value={value || ""}
        placeholder={placeholder}
        onChange={onChange}
      />
    </>
  );
};

export default Input;
