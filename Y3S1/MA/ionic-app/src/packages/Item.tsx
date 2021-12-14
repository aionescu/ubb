import React, { useState } from 'react';
import { IonButton, IonCheckbox, IonContent, IonImg, IonItem, IonLabel, IonRow } from '@ionic/react';
import { formatDate, ItemProps } from './ItemProps';
import { MyMap } from '../components/MyMap';
import { PhotoModal } from '../components/PhotoModal';

interface ItemPropsExt extends ItemProps {
  onEdit: (id?: string) => void;
}

const Item: React.FC<ItemPropsExt> = ({ _id, data: { packageName, latestVersion, uploadDate, isDeprecated, latitude, longitude, photoBase64 }, onEdit }) => {
  const [mapVisible, setMapVisible] = useState(false);

  return (
    <div>
      <IonRow>
        <IonItem onClick={() => onEdit(_id)} style={{width:"80%"}}>
          <IonLabel>{packageName}</IonLabel>
          <IonLabel>{latestVersion}</IonLabel>
          <IonLabel>{formatDate(uploadDate)}</IonLabel>
          <IonCheckbox checked={isDeprecated}/>
        </IonItem>

        <IonButton onClick={() => setMapVisible(!mapVisible)}>View Map</IonButton>
        <PhotoModal base64Data={photoBase64}/>
      </IonRow>

      { mapVisible &&
        <MyMap
          lat={latitude ?? 0}
          lng={longitude ?? 0}
        />
      }
    </div>
  );
};

export default Item;
