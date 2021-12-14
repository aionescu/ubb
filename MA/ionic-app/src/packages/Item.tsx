import React, { useState } from 'react';
import { IonButton, IonCheckbox, IonContent, IonImg, IonItem, IonLabel, IonRow } from '@ionic/react';
import { formatDate, ItemProps } from './ItemProps';
import { MyMap } from '../components/MyMap';

interface ItemPropsExt extends ItemProps {
  onEdit: (id?: string) => void;
}

const Item: React.FC<ItemPropsExt> = ({ _id, data: { packageName, latestVersion, uploadDate, isDeprecated, latitude, longitude, photoBase64 }, onEdit }) => {
  const [mapVisible, setMapVisible] = useState(false);
  const [photoVisible, setPhotoVisible] = useState(false);

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
        <IonButton onClick={() => setPhotoVisible(!photoVisible)}>View Photo</IonButton>
      </IonRow>

      { mapVisible &&
        <MyMap
          lat={latitude ?? 0}
          lng={longitude ?? 0}
        />
      }

      { photoVisible &&
        <IonItem>
          <IonImg src={"data:image/jpeg;base64," + photoBase64}/>
        </IonItem>
        }
    </div>
  );
};

export default Item;
