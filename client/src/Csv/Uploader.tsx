import React, { FormEvent } from 'react'
import axios from 'axios'

import { Row } from 'Lib/Row'

type PropsType = { }

export const Uploader : React.FC<PropsType> = props => {
    const formRef = React.useRef()

    const onSubmit = (event : FormEvent) => {
        event.preventDefault ()
        const data = new FormData (formRef.current)
        axios.post ('/csv/upload', data)
            .catch (err => {
                alert ("an error occurred")
                throw err
            })
    }

    return (
        <Row>
            <span>Choose a CSV file to upload.</span>
            <form action="#" onSubmit={ onSubmit } ref={ formRef }>
                <input type="file" name="csvFile"/>
                <input type="submit"/>
            </form>
        </Row>
    )
}
