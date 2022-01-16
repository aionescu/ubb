import React, { useContext, useState } from 'react';
import { RouteComponentProps } from 'react-router';
import {
  IonButton,
  IonCheckbox,
  IonContent,
  IonFab,
  IonFabButton,
  IonHeader,
  IonIcon,
  IonInfiniteScroll,
  IonInfiniteScrollContent,
  IonItem,
  IonList, IonLoading,
  IonPage,
  IonRow,
  IonSearchbar,
  IonSelect,
  IonSelectOption,
  IonTitle,
  IonToolbar
} from '@ionic/react';
import { add } from 'ionicons/icons';
import Item from './Item';
import { getLogger } from '../core';
import { ItemContext } from './ItemProvider';
import { useNetwork } from './Network';
import { getItems } from './ItemApi';
import { AuthContext, logout } from '../auth';
import { filterItems } from './ItemProps';

const log = getLogger('ItemList');

const showNetwork = (status: any) => status.connected ? "Online 🔵" : "OFFLINE 🔴"

const ItemList: React.FC<RouteComponentProps> = ({ history }) => {
  const { items, fetching, fetchingError } = useContext(ItemContext);
  const { token } = useContext(AuthContext);

  const { networkStatus } = useNetwork();
  const [nameFilter, setNameFilter] = useState<string | undefined>(undefined);
  const [versionFilter, setVersionFilter] = useState<string | undefined>(undefined);
  const [dateFilter, setDateFilter] = useState<string | undefined>(undefined);
  const [deprecatedFilter, setDeprecatedFilter] = useState<boolean | undefined>(undefined);
  const [scrollDisabled, setScrollDisabled] = useState<boolean>(false);
  const [page, setPage] = useState<number>(0);
  const pageSize = 10;

  function onScroll(e: CustomEvent<void>) {
    log("Scrolling")

    if (!networkStatus.connected) {
      log("Offline: Can't load more packages");
      (e.target! as HTMLIonInfiniteScrollElement).complete();
      return;
    }

    getItems(token, page + 1).then(newItems => {
      items!.push(...newItems);
      setPage(page + 1);
      setScrollDisabled(newItems.length < pageSize);
      (e.target! as HTMLIonInfiniteScrollElement).complete();
    })
  }

  return (
    <IonPage>
      <IonHeader>
        <IonToolbar>
          <IonRow>
            <IonTitle>Package Manager - {showNetwork(networkStatus)}</IonTitle>
            <IonButton onClick={logout}>Logout</IonButton>
          </IonRow>
        </IonToolbar>
      </IonHeader>
      <IonContent>
        <IonLoading isOpen={fetching} message="Fetching items" />
        <IonRow>
          <IonSearchbar
            value={nameFilter}
            debounce={100}
            onIonChange={e => setNameFilter(e.detail.value!)}
            style={{width:"25%"}}
          >
          </IonSearchbar>
          <IonSearchbar
            value={versionFilter}
            debounce={100}
            onIonChange={e => setVersionFilter(e.detail.value!)}
            style={{width:"25%"}}
          >
          </IonSearchbar>
          <IonSearchbar
            value={dateFilter}
            debounce={100}
            onIonChange={e => setDateFilter(e.detail.value!)}
            style={{width:"25%"}}
          >
          </IonSearchbar>
          <IonItem style={{width:"25%"}}>
            <IonCheckbox
              checked={deprecatedFilter}
              onIonChange={e => setDeprecatedFilter(e.detail.checked ?? false)}
            />
          </IonItem>
        </IonRow>
        {items && (
          <IonList>
            {filterItems(items, deprecatedFilter ?? false, nameFilter, versionFilter, dateFilter)
              .map(({_id, data}) =>
                <Item key={_id} _id={_id} data={data} onEdit={id => history.push(`/item/${id}`)} />)}
          </IonList>
        )}
        <IonInfiniteScroll threshold="100px" disabled={scrollDisabled}
                           onIonInfinite={onScroll}>
          <IonInfiniteScrollContent
            loadingText="Loading more packages...">
          </IonInfiniteScrollContent>
        </IonInfiniteScroll>
        {fetchingError && (
          <div>{fetchingError.message || 'Failed to fetch items'}</div>
        )}
        <IonFab vertical="bottom" horizontal="end" slot="fixed">
          <IonFabButton onClick={() => history.push('/item')}>
            <IonIcon icon={add} />
          </IonFabButton>
        </IonFab>
      </IonContent>
    </IonPage>
  );
};

export default ItemList;
