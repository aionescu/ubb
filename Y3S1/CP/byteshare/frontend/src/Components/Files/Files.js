import React, {useEffect, useState} from 'react'
import Header from "../Header/Header";
import ApiCalls from "../Api/ApiCalls";
import {Tab, Tabs, TabList, TabPanel} from 'react-tabs';
import 'react-tabs/style/react-tabs.css';
import Environment from "../Environment/Environment";
import ModalRenameFile from './ModalRenameFile';
import styles from './Files.module.css';

const Files = () => {
    const [files, setFiles] = useState([]);
    const [createdFile, setCreatedFile] = useState(false);
    const [showRenameModal, setShowRenameModal] = useState(false);
    const [fileToRename, setFileToRename] = useState('');

    function getAllFiles() {

        console.log("Getting all files")
        console.log('Authorization: ' + ApiCalls.generateTokenHeader(localStorage.getItem('token')))

        fetch(ApiCalls.host + "files", {
            method: 'GET',
            headers: {
                'Authorization': ApiCalls.generateTokenHeader(localStorage.getItem('token'))
            }
        }).then(result => {
            if (result.status === 200) {
                result.json()
                    .then(res => {
                            console.log(res.files);
                            res.files.forEach(file => {
                                localStorage.setItem(file.filename + "_filetype", file.filetype)
                            })
                            setFiles(res.files);
                        }
                    );
                console.log("SUCCESSFUL FILE FETCH!")
            } else {
                console.log("FAILED FILE FETCH!")
                //TODO: Add error for invalid fetch
            }
        })
    }

    function saveFile(filename, contents) {
        let fileType = localStorage.getItem(filename + "_filetype")
        if (fileType === undefined || fileType === null) {
            fileType =  ".py"
        }

        console.log("FILE TYPE: " + fileType)
        console.log("Saving file: " + filename + " with contents: " + contents)
        console.log('Authorization: ' + ApiCalls.generateTokenHeader(localStorage.getItem('token')))

        fetch(ApiCalls.host + "files/save", {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Authorization': ApiCalls.generateTokenHeader(localStorage.getItem('token'))
            },
            body: JSON.stringify({"filename": filename, "contents": contents, "filetype": fileType})
        }).then(result => {
            if (result.status === 200) {
                console.log("SUCCESSFUL FILE SAVE!")
            } else {
                console.log("FAILED FILE SAVE!")
                //TODO: Add error for invalid register
            }
        })
    }

    useEffect(getAllFiles, []);

    function newTab() {
        setCreatedFile(true);
    }

    function renameFile() {
        // TO DO: call backend and persist renaming of the file in tabs
        setShowRenameModal(false);
    }

    useEffect(() => {
        if (createdFile) {
            const newFile = {filename: "new" + files.length, contents: ""};
            setFiles([...files, newFile])
            saveFile(newFile.filename, newFile.contents);
            setCreatedFile(false);
        }
    }, [createdFile])

    function openRenameModal(filename) {
        setFileToRename(filename)
        setShowRenameModal(true)
    }

    return (
        <React.Fragment>
            <Header/>
            <ModalRenameFile key={fileToRename} allFiles={files} filename={fileToRename} show={showRenameModal}
                             close={renameFile}/>
            <Tabs>
                <TabList className={styles.tabs}>
                    {
                        files && files.map((file) =>
                            <Tab key={file.filename}
                                 onDoubleClick={() => openRenameModal(file.filename)}>
                                {file.filename}
                            </Tab>)
                    }
                    <Tab onClick={() => newTab()}>+</Tab>
                </TabList>

                {
                    files && files.map((file) =>
                        <TabPanel key={file.filename}>
                            <Environment file={file}/>
                        </TabPanel>
                    )

                }
                <TabPanel><Environment/></TabPanel>
            </Tabs>
        </React.Fragment>
    )
}
export default Files