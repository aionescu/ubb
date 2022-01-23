import React, {useEffect, useState} from 'react'
import styles from './GuestEnvironment.module.css'
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
import {useNavigate} from "react-router-dom";

const GuestEnvironment = (props) => {
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

    const downloadFile = () => {
        const element = document.createElement('a')
        const blob = new Blob([code], {type:'text/plain'})
        element.href = URL.createObjectURL(blob)
        element.download = "byteshare" + filetype
        document.body.appendChild(element)
        element.click()
        // URL.revokeObjectURL(element.href)

    }

    const [language, setLanguage] = useState(languages[0].value)
    const [filetype, setFileType] = useState(languages[0].filetype)
    const [theme, setTheme] = useState(themes[0].value)
    const [code, setCode] = useState("")

    const languageChangeHandle = (e) => {
        console.log("Filtering " + e.target.value + " ???")
        setFileType(languages.filter(l => l.value === e.target.value)[0].filetype)
        setLanguage(e.target.value)
    }

    const themeChangeHandle = (e) => {
        setTheme(e.target.value)
    }

    useEffect(() => {
        console.log(language)
    }, [language])

    const onEditorChange = (newValue) => {
        setCode(newValue)
    }

    return(
        <React.Fragment>
            <Header/>
            <select id={"languages-select"} onChange={languageChangeHandle}>
                {languages.map( (language) => (
                    <option key={language.value} value={language.value}>{language.label}</option>
                ))}
            </select>
            <select id={"themes-select"} onChange={themeChangeHandle}>
                {themes.map((theme) => (
                    <option key={theme.value} value={theme.value}>{theme.label}</option>
                ))}
            </select>
            <button className={styles.download_file_button} onClick={() => downloadFile()}>Download File</button>

            <article className={styles.CodeContainer}>
                <AceEditor
                    style={{width: 2000, height: 900}}
                    placeholder={"Write your code here"}
                    onChange={onEditorChange}
                    mode={language}
                    theme={theme}
                    name="UNIQUE_ID_OF_DIV"
                    fontSize={24}
                    editorProps={{ $blockScrolling: true }}
                />
            </article>
        </React.Fragment>
    )
}
export default GuestEnvironment