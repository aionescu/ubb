import React, { useContext, useState } from 'react';
import { RouteComponentProps } from 'react-router';
import {
  IonContent,
  IonFab,
  IonFabButton,
  IonHeader,
  IonIcon,
  IonInfiniteScroll,
  IonInfiniteScrollContent,
  IonList, IonLoading,
  IonPage,
  IonSearchbar,
  IonTitle,
  IonToolbar
} from '@ionic/react';
import { add } from 'ionicons/icons';
import Item from './Item';
import { getLogger } from '../core';
import { ItemContext } from './ItemProvider';
import { useNetwork } from './Network';
import { getItems } from './ItemApi';
import { AuthContext } from '../auth';

const log = getLogger('ItemList');

const showNetwork = (status: any) => status.connected ? "Online 🔵" : "OFFLINE 🔴"

const ItemList: React.FC<RouteComponentProps> = ({ history }) => {
  const { items, fetching, fetchingError } = useContext(ItemContext);
  const { token } = useContext(AuthContext);

  const { networkStatus } = useNetwork();
  const [searchString, setSearchString] = useState<string>("");
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
          <IonTitle>Package Manager - {showNetwork(networkStatus)}</IonTitle>
        </IonToolbar>
      </IonHeader>
      <IonContent>
        <IonLoading isOpen={fetching} message="Fetching items" />
        <IonSearchbar
          value={searchString}
          debounce={100}
          onIonChange={e => setSearchString(e.detail.value!)}>
        </IonSearchbar>
        {items && (
          <IonList>
            {items
              .filter(item => item.data.packageName.indexOf(searchString) > -1)
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
