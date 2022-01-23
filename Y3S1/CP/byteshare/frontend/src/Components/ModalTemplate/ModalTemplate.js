import React from 'react'
import styles from './ModalTemplate.module.css'
import Backdrop from "../Backdrop/Backdrop";

const ModalTemplate = (props) => {
    return (
        <React.Fragment>
            <Backdrop show={props.show} onClose={props.onClose}/>
            <div className={`${styles.modal} ${props.className ? props.className : ''}`}
                style={{
                    transform: props.show ? 'translateY(0)':'translateY(-100vh)',
                    opacity: props.show ? '1' : '0'
                }}>
                <span className={styles.close_button} onClick={props.onClose}>&times;</span>
                {props.children}
            </div>
        </React.Fragment>

    )
}
export default ModalTemplate