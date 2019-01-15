import React from 'react';
import { ToastContainer as Container, toast, Zoom } from 'react-toastify';


function ToastContainer(props) {
  return <Container {...props}
                    className="toastContainer"
                    toastClassName="toast"
                    autoClose={false}
                    transition={Zoom}
         />;
}


function info() {
  console.log({...arguments});
  toast.info({...arguments});
}

function error() {
  console.error({...arguments});
  toast.error({...arguments});
}


export {ToastContainer, info, error}
