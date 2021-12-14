import React, { useContext, useEffect, useState } from 'react';
import {
  IonButton,
  IonButtons,
  IonCheckbox,
  IonContent,
  IonDatetime,
  IonHeader,
  IonImg,
  IonInput,
  IonItem,
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
import { MyMap } from '../components/MyMap';
import { useMyLocation } from '../hooks/useMyLocation';
import { Photo, usePhotoGallery } from '../hooks/usePhotoGallery';

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
  const [mapVisible, setMapVisible] = useState(false);
  const [photoVisible, setPhotoVisible] = useState(false);
  const myLocation = useMyLocation();
  const { photos, takePhoto, takePhotoBase64 } = usePhotoGallery();

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
        <br/>

        <IonButton onClick={() => {
          let loc = myLocation.position?.coords;
          setData({ ...data, latitude: loc?.latitude ?? 0, longitude: loc?.longitude ?? 0 });
        }}>Use My Location</IonButton>

        <IonButton onClick={() => setMapVisible(!mapVisible)}>Edit Location</IonButton>
        <br/>

        <IonButton onClick={
          async () => setData({ ...data, photoBase64: await takePhotoBase64(item?._id ?? "Unknown") })
        }>Take Photo</IonButton>

        <IonButton onClick={() => setPhotoVisible(!photoVisible)}>View Photo</IonButton>
        <br/>

        { mapVisible &&
          <MyMap
            lat={data.latitude ?? 0}
            lng={data.longitude ?? 0}
            onMapClick={
              (e: any) => {
                console.log(e.latLng.lat())
                console.log(e.latLng.lng())
                setData({ ...data, latitude: e.latLng.lat(), longitude: e.latLng.lng() })
              }
            }
            onMarkerClick={log('onMarker')}
          />
        }

        { photoVisible &&
          <IonItem>
            <IonImg src={"data:image/jpeg;base64," + data.photoBase64}/>
          </IonItem>
        }

        <IonLoading isOpen={saving} />
        {savingError && (
          <div>{savingError.message || 'Failed to save item'}</div>
        )}
      </IonContent>
    </IonPage>
  );
};

export default ItemEdit;
