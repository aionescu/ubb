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
import { defaultItemData, ItemData, ItemProps } from './ItemProps';

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
  const [data, setData] = useState(defaultItemData);
  const [item, setItem] = useState<ItemProps>();
  useEffect(() => {
    log('useEffect');
    const routeId = match.params.id || '';
    const item = items?.find(it => it._id === routeId);
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
        <IonLabel>Package Name:</IonLabel>
        <IonInput
          value={data.packageName}
          onIonChange={e => setData({ ...data, packageName: e.detail.value! })}
        />

        <IonLabel>Latest Version:</IonLabel>
        <IonInput
          value={Number.isNaN(data.latestVersion) ? "" : data.latestVersion}
          onIonChange={e => setData({ ...data, latestVersion: parseNum(e.detail.value!, data.latestVersion) })}
        />

        <IonLabel>Upload Date:</IonLabel>
        <IonDatetime
          displayFormat="YYYY-MM-DD"
          placeholder="Edit Upload Date"
          value={data.uploadDate.toString()}
          onIonChange={e => setData({ ...data, uploadDate: new Date(e.detail.value!) })}>
        </IonDatetime>

        <IonLabel>Is Deprecated?</IonLabel>
        <br/>
        <IonCheckbox
          checked={data.isDeprecated}
          onIonChange={e => setData({ ...data, isDeprecated: e.detail.checked })}
        />

        <IonLoading isOpen={saving} />
        {savingError && (
          <div>{savingError.message || 'Failed to save item'}</div>
        )}
      </IonContent>
    </IonPage>
  );
};

export default ItemEdit;
