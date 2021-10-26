import React, { useContext, useEffect, useState } from 'react';
import {
  IonButton,
  IonButtons,
  IonCheckbox,
  IonContent,
  IonDatetime,
  IonHeader,
  IonInput,
  IonLabel,
  IonLoading,
  IonPage,
  IonTitle,
  IonToolbar
} from '@ionic/react';
import { getLogger } from '../core';
import { ItemContext } from './ItemProvider';
import { RouteComponentProps } from 'react-router';
import { ItemProps } from './ItemProps';

const log = getLogger('ItemEdit');

interface ItemEditProps extends RouteComponentProps<{
  id?: string;
}> {}

const parseNum = (str: string, num: number) => {
  const number = Number(str)
  return Number.isNaN(number) ? num : number
}

const ItemEdit: React.FC<ItemEditProps> = ({ history, match }) => {
  const { items, saving, savingError, saveItem } = useContext(ItemContext);
  const [data, setData] = useState({ num: 0, str: "", date: new Date(Date.now()), bool: false });
  const [item, setItem] = useState<ItemProps>();
  useEffect(() => {
    log('useEffect');
    const routeId = match.params.id || '';
    const item = items?.find(it => it.id === routeId);
    setItem(item);
    if (item) {
      setData(item.data);
    }
  }, [match.params.id, items]);
  const handleSave = () => {
    const editedItem = item ? { ...item, data } : { data };
    saveItem && saveItem(editedItem).then(() => history.goBack());
  };
  // log('render');
  return (
    <IonPage>
      <IonHeader>
        <IonToolbar>
          <IonTitle>Edit</IonTitle>
          <IonButtons slot="end">
            <IonButton onClick={handleSave}>
              Save
            </IonButton>
          </IonButtons>
        </IonToolbar>
      </IonHeader>
      <IonContent>
        <IonLabel>Num:</IonLabel>
        <IonInput value={Number.isNaN(data.num) ? "" : data.num} onIonChange={e => setData({ ...data, num: parseNum(e.detail.value || '', data.num) })} />

        <IonLabel>Str:</IonLabel>
        <IonInput value={data.str} onIonChange={e => setData({ ...data, str: e.detail.value || '' })} />

        <IonLabel>Date:</IonLabel>
        <IonDatetime displayFormat="YYYY-MM-DD" placeholder="Edit Date" value={data.date.toString()} onIonChange={e => setData({ ...data, date: new Date(e.detail.value || '') })}></IonDatetime>

        <IonLabel>Bool:</IonLabel>
        <br/>
        <IonCheckbox checked={data.bool} onIonChange={e => setData({ ...data, bool: e.detail.checked })} />

        <IonLoading isOpen={saving} />
        {savingError && (
          <div>{savingError.message || 'Failed to save item'}</div>
        )}
      </IonContent>
    </IonPage>
  );
};

export default ItemEdit;
