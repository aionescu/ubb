import React, { useContext, useState } from 'react';
import { RouteComponentProps } from 'react-router';
import {
  IonAlert,
  IonContent,
  IonFab,
  IonFabButton,
  IonHeader,
  IonIcon,
  IonInput,
  IonLabel,
  IonList, IonLoading,
  IonPage,
  IonTitle,
  IonToolbar,
  useIonAlert
} from '@ionic/react';
import { add } from 'ionicons/icons';
import Item from './Item';
import { getLogger } from '../core';
import { ItemProps } from './ItemProps';
import { getItems, updateItem } from './ItemApi';

const log = getLogger('ItemList');

const ItemList: React.FC<RouteComponentProps> = ({ history }) => {
  const [query, setQuery] = useState<string>("");
  const [items, setItems] = useState<ItemProps[]>([]);
  const [fetching, setFetching] = useState<boolean>(false);
  const [showAlert] = useIonAlert();

  async function updateQuery(q: string) {
    if (q.length > 0) {
      setQuery(q);
      setFetching(true);

      try {
        const items = await getItems(q);
        setItems(items);
      } catch (e) {
        showAlert("Error fetching tasks");
      }

      setFetching(false);
    }
  }

  async function flip({ id, text, status, version }: ItemProps) {
    setFetching(true);

    try {
      await updateItem({ id, text, version, status: status === "active" ? "done" : "active" });
    } catch (e) {
      showAlert("Version conflict");
    }

    try {
      const items = await getItems(query);
      setItems(items);
    } catch (e) {
      showAlert("Error fetching tasks");
    }

    setFetching(false);
  }

  return (
    <IonPage>
      <IonHeader>
        <IonToolbar>
          <IonTitle>Ionic Exam</IonTitle>
        </IonToolbar>
      </IonHeader>
      <IonContent>
        <IonLoading isOpen={fetching} message="Please wait..." />

        <IonLabel>Query:</IonLabel>
        <IonInput value={query} onIonChange={async e => await updateQuery(e.detail.value ?? "")} style={{backgroundColor: "cyan"}}/>

        <IonLabel>Tasks:</IonLabel>
        {items && (
          <IonList>
            {items.map(({ id, text, status, version}) =>
              <Item key={id} id={id} text={text} status={status} version={version} flip={flip}/>)}
          </IonList>
        )}
        {/* <IonFab vertical="bottom" horizontal="end" slot="fixed">
          <IonFabButton onClick={() => history.push('/item')}>
            <IonIcon icon={add} />
          </IonFabButton>
        </IonFab> */}
      </IonContent>
    </IonPage>
  );
};

export default ItemList;
