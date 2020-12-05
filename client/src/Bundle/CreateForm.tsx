import React from 'react'

import { create } from 'Api/Bundle'
import style from './CreateFormStyle.scss'

type PropsType = {
    onCreate?: () => void
    onCreated?: () => void
}

export const CreateForm: React.FC<PropsType> = props => {
    const [name, setName] = React.useState("")
    const [desc, setDesc] = React.useState("")

    const onNameChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        setName(event.target.value)
    }
    const onDescChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        setDesc(event.target.value)
    }

    const onSubmit = async (event: React.FormEvent) => {
        event.preventDefault()
        if (props.onCreate) props.onCreate()
        await create(name, desc)
        if (props.onCreated) props.onCreated()
    }

    return (
        <form onSubmit={ onSubmit }>
            <label className={ style.item }>
                name
                <input name="name" onChange={ onNameChange } value={ name }></input>
            </label>
            <label className={ style.item }>
                description
                <input name="desc" onChange={ onDescChange } value={ desc }></input>
            </label>
            <input type="submit" value="Create Bundle"></input>
        </form>
    )
}
