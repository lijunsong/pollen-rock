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

export {ToastContainer, toast as notify}
