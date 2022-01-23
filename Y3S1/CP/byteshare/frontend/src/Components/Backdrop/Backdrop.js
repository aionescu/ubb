import React from 'react'
import styles from './Backdrop.module.css'
const Backdrop = (props) => {
    return (
        props.show ? <div className={styles.backdrop} onClick={props.onClose}/> : null
    )
}
export default Backdrop