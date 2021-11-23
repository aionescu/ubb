import { useContext, useEffect, useState } from 'react';
import { NetworkStatus, Plugins } from '@capacitor/core';
import { AuthContext } from '../auth';
import { sendItems } from './ItemProvider';

const { Network } = Plugins;

const initialState = {
  connected: false,
  connectionType: 'unknown',
}

export const useNetwork = () => {
  const [networkStatus, setNetworkStatus] = useState(initialState)
  const { token } = useContext(AuthContext);

  useEffect(() => {
    const handler = Network.addListener('networkStatusChange', handleNetworkStatusChange);
    Network.getStatus().then(handleNetworkStatusChange);
    let canceled = false;
    return () => {
      canceled = true;
      handler.remove();
    }

    function handleNetworkStatusChange(status: NetworkStatus) {
      console.log('useNetwork - status change', status);
      if (!canceled) {
        setNetworkStatus(status);
      }
      sendItems(token, status)
    }
  }, [token])
  return { networkStatus };
};
