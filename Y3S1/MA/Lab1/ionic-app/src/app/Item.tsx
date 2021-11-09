import React from 'react';
import { IonCheckbox, IonItem, IonLabel } from '@ionic/react';
import { formatDate, ItemProps } from './ItemProps';

interface ItemPropsExt extends ItemProps {
  onEdit: (id?: string) => void;
}

const Item: React.FC<ItemPropsExt> = ({ id, data: { packageName, latestVersion, uploadDate, isDeprecated }, onEdit }) => {
  return (
    <IonItem onClick={() => onEdit(id)}>
      <IonLabel>Package #{id}:</IonLabel>
      <IonLabel>{packageName}</IonLabel>
      <IonLabel>{latestVersion}</IonLabel>
      <IonLabel>{formatDate(uploadDate)}</IonLabel>
      <IonCheckbox checked={isDeprecated}/>
    </IonItem>
  );
};

export default Item;
