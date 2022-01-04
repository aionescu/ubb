import {useCamera} from '@ionic/react-hooks/camera';
import {CameraPhoto, CameraResultType, CameraSource, FilesystemDirectory} from '@capacitor/core';
import {useEffect, useState} from 'react';
import {base64FromPath, useFilesystem} from '@ionic/react-hooks/filesystem';
import {useStorage} from '@ionic/react-hooks/storage';

export interface Photo {
    filepath: string;
    webviewPath?: string;
}

const PHOTO_STORAGE = 'photos';

export function usePhotoGallery() {
    const {getPhoto} = useCamera();
    const [photos, setPhotos] = useState<Photo[]>([]);
    const {get, set } = useStorage();

    const takePhotoBase64 = async (id: string) => {
      const cameraPhoto = await getPhoto({
          resultType: CameraResultType.Base64,
          source: CameraSource.Camera,
          quality: 100
      });

      const base64Data = cameraPhoto.base64String ?? ""

      await writeFile({
        path: "photo_" + id + ".png",
        data: base64Data,
        directory: FilesystemDirectory.Documents
      });

      await set("photo_" + id, base64Data)

      return base64Data
  };

    const takePhoto = async (filename: string) => {
        const cameraPhoto = await getPhoto({
            resultType: CameraResultType.Uri,
            source: CameraSource.Camera,
            quality: 100
        });
        const fileName = filename + '.jpeg';

        const existingPhoto = photos.find(p => p.filepath.split(".")[0] === filename);
        const path = existingPhoto?.filepath.substr(existingPhoto.filepath.lastIndexOf('/') + 1);
        path && await deleteFile({
            path,
            directory: FilesystemDirectory.Data
        });

        const savedFileImage = await savePicture(cameraPhoto, fileName);

        const rest = photos.filter(p => !p.filepath.includes(filename));
        const newPhotos = [savedFileImage, ...rest];
        setPhotos(newPhotos);
        await set(PHOTO_STORAGE, JSON.stringify(newPhotos));
    };

    const {deleteFile, readFile, writeFile} = useFilesystem();
    const savePicture = async (photo: CameraPhoto, fileName: string): Promise<Photo> => {
        const base64Data = await base64FromPath(photo.webPath ?? "");
        await writeFile({
            path: fileName,
            data: base64Data,
            directory: FilesystemDirectory.Data
        });

        return {
            filepath: fileName,
            webviewPath: photo.webPath
        };
    };

    useEffect(() => {
        const loadSaved = async () => {
            const photosString = await get(PHOTO_STORAGE);
            const photos = (photosString ? JSON.parse(photosString) : []) as Photo[];
            for (let photo of photos) {
                const file = await readFile({
                    path: photo.filepath,
                    directory: FilesystemDirectory.Data
                });
                photo.webviewPath = `data:image/jpeg;base64,${file.data}`;
            }
            setPhotos(photos);
        };
        loadSaved();
    }, [get, readFile]);

    const deletePhoto = async (photo: Photo) => {
        console.log("delete");
        const newPhotos = photos.filter(p => p.filepath !== photo.filepath);
        set(PHOTO_STORAGE, JSON.stringify(newPhotos));
        const filename = photo.filepath.substr(photo.filepath.lastIndexOf('/') + 1);
        await deleteFile({
            path: filename,
            directory: FilesystemDirectory.Data
        });
        setPhotos(newPhotos);
    };

    return {
        photos,
        takePhoto,
        takePhotoBase64,
        deletePhoto,
    };
}
