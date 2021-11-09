import React, { useCallback, useEffect, useReducer } from 'react';
import PropTypes from 'prop-types';
import { getLogger } from '../core';
import { deserializeItem, ItemProps } from './ItemProps';
import { createItem, getItems, newWebSocket, updateItem } from './ItemApi';
import { useNetwork } from './Network';
import { Plugins } from '@capacitor/core';
const { Storage } = Plugins;

const log = getLogger('ItemProvider');

type SaveItemFn = (item: ItemProps) => Promise<any>;

export interface ItemsState {
  items?: ItemProps[],
  fetching: boolean,
  fetchingError?: Error | null,
  saving: boolean,
  savingError?: Error | null,
  saveItem?: SaveItemFn
}

interface ActionProps {
  type: string,
  payload?: any,
}

const initialState: ItemsState = {
  fetching: false,
  saving: false
};

const FETCH_ITEMS_STARTED = 'FETCH_ITEMS_STARTED';
const FETCH_ITEMS_SUCCEEDED = 'FETCH_ITEMS_SUCCEEDED';
const FETCH_ITEMS_OFFLINE = 'FETCH_ITEMS_OFFLINE';
const FETCH_ITEMS_FAILED = 'FETCH_ITEMS_FAILED';
const SAVE_ITEM_STARTED = 'SAVE_ITEM_STARTED';
const SAVE_ITEM_SUCCEEDED = 'SAVE_ITEM_SUCCEEDED';
const SAVE_ITEM_FAILED = 'SAVE_ITEM_FAILED';
const BACK_ONLINE = 'BACK_ONLINE';

const reducer: (state: ItemsState, action: ActionProps) => ItemsState =
  (state, { type, payload }) => {
    switch (type) {
      case FETCH_ITEMS_STARTED:
        return { ...state, fetching: true, fetchingError: null };
      case FETCH_ITEMS_SUCCEEDED:
        return { ...state, items: payload.items, fetching: false };
      case FETCH_ITEMS_OFFLINE:
        return { ...state, fetching: false };
      case FETCH_ITEMS_FAILED:
        return { ...state, fetchingError: payload.error, fetching: false };
      case SAVE_ITEM_STARTED:
        return { ...state, savingError: null, saving: true };
      case SAVE_ITEM_SUCCEEDED:
        const items = [...(state.items || [])];
        const item = deserializeItem(payload.item);
        const index = items.findIndex(it => it.id === item.id);
        if (index === -1) {
          items.splice(0, 0, item);
        } else {
          items[index] = item;
        }
        return { ...state, items, saving: false };
      case SAVE_ITEM_FAILED:
        return { ...state, savingError: payload.error, saving: false };
      case BACK_ONLINE:
        return { ...state, unsent: [] };
      default:
        return state;
    }
  };

export const ItemContext = React.createContext<ItemsState>(initialState);

interface ItemProviderProps {
  children: PropTypes.ReactNodeLike,
}

function nextId(items: ItemProps[]) {
  const ids = items.map(item => Number(item.id || "-1"));
  const maxId = Math.max(...ids);
  return maxId + 1;
}

function sendItems(networkStatus: any) {
  if (!networkStatus.connected)
    return;

  (async () => {
    const keys = (await Storage.keys()).keys;

    if (keys.length === 0)
      return;

    for (const key of keys) {
      const item = await Storage.get({ key });
      const itemData = JSON.parse(item.value!);
      const itemProps: ItemProps = {
        id: itemData.id,
        data: itemData.data
      };

      if (itemData.isNew)
        await createItem(itemProps);
      else
        await updateItem(itemProps);

      await Storage.remove({ key });
    }
  })()
}

export const ItemProvider: React.FC<ItemProviderProps> = ({ children }) => {
  const [state, dispatch] = useReducer(reducer, initialState);
  const { items, fetching, fetchingError, saving, savingError } = state;
  const { networkStatus } = useNetwork();

  const saveItem = useCallback<SaveItemFn>(is => saveItemCallback(items!, networkStatus, is), [networkStatus]);
  const value = { items, fetching, fetchingError, saving, savingError, saveItem };

  useEffect(() => getItemsEffect(networkStatus), [networkStatus]);
  useEffect(wsEffect, []);
  useEffect(() => sendItems(networkStatus), [networkStatus]);

  return (
    <ItemContext.Provider value={value}>
      {children}
    </ItemContext.Provider>
  );

  function getItemsEffect(networkStatus: any) {
    let canceled = false;
    fetchItems(networkStatus);
    return () => {
      canceled = true;
    }

    async function fetchItems(networkStatus: any) {
      try {
        log('fetchItems started');
        dispatch({ type: FETCH_ITEMS_STARTED });

        if (!networkStatus.connected) {
          log("Offline, can't fetch items");
          dispatch({ type: FETCH_ITEMS_OFFLINE, payload: { } });
        }

        const items = await getItems(0);
        log('fetchItems succeeded');
        if (!canceled) {
          dispatch({ type: FETCH_ITEMS_SUCCEEDED, payload: { items } });
        }
      } catch (error) {
        log('fetchItems failed');
        dispatch({ type: FETCH_ITEMS_FAILED, payload: { error } });
      }
    }
  }

  async function saveItemCallback(items: ItemProps[], networkStatus: any, item: ItemProps) {
    try {
      log('saveItem started');
      dispatch({ type: SAVE_ITEM_STARTED });

      if (!networkStatus.connected) {
        log("Offline, can't save item.")
        const itemId = item.id || nextId(items).toString();

        await Storage.set({
          key: itemId,
          value: JSON.stringify({ ...item, id: itemId, isNew: item.id === undefined })
        });

        dispatch({ type: SAVE_ITEM_SUCCEEDED, payload: { item } });
      } else {
        const savedItem = await (item.id ? updateItem(item) : createItem(item));
        log('saveItem succeeded');
        dispatch({ type: SAVE_ITEM_SUCCEEDED, payload: { item: savedItem } });
      }
    } catch (error) {
      log('saveItem failed');
      dispatch({ type: SAVE_ITEM_FAILED, payload: { error } });
    }
  }

  function wsEffect() {
    let canceled = false;
    log('wsEffect - connecting');
    const closeWebSocket = newWebSocket(message => {
      if (canceled) {
        return;
      }
      const { event, payload: { item }} = message;
      log(`ws message, item ${event}`);
      if (event === 'created' || event === 'updated') {
        dispatch({ type: SAVE_ITEM_SUCCEEDED, payload: { item } });
      }
    });
    return () => {
      log('wsEffect - disconnecting');
      canceled = true;
      closeWebSocket();
    }
  }
};
