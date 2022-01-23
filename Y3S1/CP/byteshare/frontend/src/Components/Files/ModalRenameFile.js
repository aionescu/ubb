import React, {useState} from 'react'
import ModalTemplate from "../ModalTemplate/ModalTemplate";
import styles from './Files.module.css';
import ApiCalls from "../Api/ApiCalls";

const ModalRenameFile = (props) => {
    const [filename, setFilename] = useState(props.filename);
    const [allFiles, setAllFiles] = useState(props.allFiles);

    function test(newValue) {
    }

    function renameFile(filename, newFilename, event) {
        try {localStorage.setItem(newFilename + "_language", localStorage.getItem(filename + "_language"))} catch {}
        try {localStorage.setItem(newFilename + "_theme", localStorage.getItem(filename + "_theme"))} catch {}
        try {localStorage.setItem(newFilename + "_filetype", localStorage.getItem(filename + "_filetype"))} catch {}

        try {localStorage.getItem(filename + "_language")} catch {}
        try {localStorage.removeItem(filename + "_theme")} catch {}
        try {localStorage.removeItem(filename + "_filetype")} catch {}

        console.log("Renaming file: " + filename + " to: " + newFilename)
        console.log('Authorization: ' + ApiCalls.generateTokenHeader(localStorage.getItem('token')))

        fetch(ApiCalls.host + "files/rename", {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Authorization': ApiCalls.generateTokenHeader(localStorage.getItem('token'))
            },
            body: JSON.stringify({"filename": filename, "newFilename": newFilename})
        }).then(result => {
            if (result.status === 200) {
                console.log("SUCCESSFUL FILE RENAME!")
            } else {
                console.log("FAILED FILE RENAME!")
            }
        })

        setFilename(newFilename)
        let i = allFiles.findIndex(x => x.filename === props.filename)
        if (i !== undefined) {
            props.allFiles[i].filename = newFilename
            setAllFiles(props.allFiles)
        }

        props.close()
    }

    return (
        <ModalTemplate className={styles.rename_modal} show={props.show} onClose={props.close}>
            <h1>Rename File</h1>
            <input type={"text"} placeholder={"File Name"} value={filename}
                   onInput={event => setFilename(event.target.value)}/>
            <button onClick={event => {
                renameFile(props.filename, filename);
            }}>Rename
            </button>
        </ModalTemplate>
    )
}
export default ModalRenameFile