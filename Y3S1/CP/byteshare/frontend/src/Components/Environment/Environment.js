import React, {useEffect, useState} from 'react'
import styles from './Environment.module.css'
import AceEditor from "react-ace";
import "ace-builds/src-noconflict/mode-python";
import "ace-builds/src-noconflict/mode-javascript";
import "ace-builds/src-noconflict/mode-ruby";
import "ace-builds/src-noconflict/mode-csharp";
import "ace-builds/src-noconflict/theme-github";
import "ace-builds/src-noconflict/theme-monokai";
import "ace-builds/src-noconflict/theme-tomorrow";
import "ace-builds/src-noconflict/theme-kuroir";
import "ace-builds/src-noconflict/theme-twilight";
import "ace-builds/src-noconflict/theme-xcode";
import "ace-builds/src-noconflict/theme-textmate";
import "ace-builds/src-noconflict/theme-solarized_dark";
import "ace-builds/src-noconflict/theme-solarized_light";
import "ace-builds/src-noconflict/theme-terminal";
import Header from "../Header/Header";
import ApiCalls from "../Api/ApiCalls";

const Environment = ({file}) => {
    const languages = [
        {
            label: 'Python',
            value: 'python',
            filetype: '.py'
        },
        {
            label: 'Javascript',
            value: 'javascript',
            filetype: '.js'
        },
        {
            label: 'Java',
            value: 'java',
            filetype: '.class'
        },
        {
            label: 'Ruby',
            value: 'ruby',
            filetype: '.rb'
        },
        {
            label: 'C#',
            value: 'csharp',
            filetype: '.cs'
        },
    ]

    const themes = [
        {
            label: 'Github',
            value: 'github',
        },
        {
            label: 'Monokai',
            value: 'monokai',
        },
        {
            label: 'Tomorrow',
            value: 'tomorrow',
        },
        {
            label: 'Kuroir',
            value: 'kuroir',
        },
        {
            label: 'Twilight',
            value: 'twilight',
        },
        {
            label: 'Xcode',
            value: 'xcode',
        },
        {
            label: 'Textmate',
            value: 'textmate',
        },
        {
            label: 'Solarized Dark',
            value: 'solarized_dark',
        },
        {
            label: 'Solarized Light',
            value: 'solarized_light',
        },
        {
            label: 'Terminal',
            value: 'terminal',
        },
    ]

    let initialTheme = {value: themes[0].value, label: themes[0].label};
    let initialLanguage = {value: languages[0].value, label: languages[0].label};
    let initialFiletype = ".py"

    if (file && file.filename) {
        console.log("ENTERED HERE 0.5")
        if (localStorage.getItem(file.filename))
            file.contents = localStorage.getItem(file.filename)
        if (localStorage.getItem(file.filename + "_theme")) {
            initialTheme = themes.find(t => t.value === localStorage.getItem(file.filename + "_theme"))
        }
        if (localStorage.getItem(file.filename + "_language")) {
            initialLanguage = languages.find(l => l.value === localStorage.getItem(file.filename + "_language"))
        }
        if (localStorage.getItem(file.filename + "_filetype")) {
            initialFiletype = localStorage.getItem(file.filename + "_filetype")
            console.log("ENTERED HERE 2")
        } else {
            localStorage.setItem(file.filename + "_filetype", ".py")
            console.log("ENTERED HERE 3")
            initialFiletype = ".py"
        }
    }

    const [language, setLanguage] = useState(initialLanguage)
    const [theme, setTheme] = useState(initialTheme)
    const [filetype, setFiletype] = useState(initialFiletype)

    const [code, setCode] = useState(file && file.contents && file.filename ? file.contents : "")

    const languageChangeHandle = (e) => {
        const newLanguage = languages.find(l => l.value === e.target.value)

        setFiletype(newLanguage.filetype)
        localStorage.setItem(file.filename + "_filetype", newLanguage.filetype)
        setLanguage(newLanguage)
    }

    const themeChangeHandle = (e) => {
        const newTheme = themes.find(t => t.value === e.target.value)
        setTheme(newTheme)
    }


    const onEditorChange = (newValue) => {
        setCode(newValue)
    }

    const downloadFile = () => {
        const element = document.createElement('a')
        const blob = new Blob([code], {type: 'text/plain'})
        element.href = URL.createObjectURL(blob)
        element.download = file.filename + language?.filetype
        document.body.appendChild(element)
        element.click()
        // URL.revokeObjectURL(element.href)

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

    function deleteFile(filename) {

        console.log("Deleting file: " + filename)
        console.log('Authorization: ' + ApiCalls.generateTokenHeader(localStorage.getItem('token')))


        fetch(ApiCalls.host + "files/delete", {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Authorization': ApiCalls.generateTokenHeader(localStorage.getItem('token'))
            },
            body: JSON.stringify({"filename": filename})
        }).then(result => {
            if (result.status === 200) {
                console.log("SUCCESSFUL FILE DELETE!")
            } else {
                console.log("FAILED FILE DELETE!")
                //TODO: Add error for invalid delete
            }
        })
    }

    useEffect(() => {
        if (file && file.filename) {
            localStorage.setItem(file.filename, code);
        }
    }, [code])

    useEffect(() => {
        if (file && file.filename && theme) {
            localStorage.setItem(file.filename + "_theme", theme.value)
        }
    }, [theme])

    useEffect(() => {
        if (file && file.filename && language) {
            localStorage.setItem(file.filename + "_language", language.value)
            localStorage.setItem(file.filename + "_filetype", language.filetype)
        }
    }, [language])

    useEffect(() => {
        return () => {
            if (file && file.filename) {
                if (localStorage.getItem(file.filename)) {
                    saveFile(file.filename, localStorage.getItem(file.filename))
                }
            }
        }
    }, [])

    return (
        <React.Fragment>
            <select id={"languages-select"} onChange={languageChangeHandle} defaultValue={language?.value}>
                {languages.map((l) => (
                    <option key={l.value} value={l.value}>{l.label}</option>
                ))}
            </select>
            <select id={"themes-select"} onChange={themeChangeHandle} defaultValue={theme?.value}>
                {themes.map((t) => (
                    <option key={t.value} value={t.value}>{t.label}</option>
                ))}
            </select>
            <button className={styles.download_file_button} onClick={() => downloadFile()}>Download File</button>
            {
                //<button onClick={() => saveFile("make_me_dynamic.py", "get the value from the textfield")}>Test save</button>
                //<button onClick={() => deleteFile("make_me_dynamic.py")}>Test delete</button>
            }
            {
                <article className={styles.CodeContainer}>
                    <AceEditor
                        style={{width: 2000, height: 900}}
                        placeholder={"Write your code here"}
                        value={code}
                        onChange={onEditorChange}
                        mode={language?.value}
                        theme={theme?.value}
                        name="UNIQUE_ID_OF_DIV"
                        fontSize={24}
                        editorProps={{$blockScrolling: true}}
                    />
                </article>
            }


        </React.Fragment>
    )
}
export default Environment