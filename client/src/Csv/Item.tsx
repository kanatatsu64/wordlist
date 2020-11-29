import React from 'react'

type PropsType = {
    name: String,
    onSelect: (name: String) => void,
    onDelete: (name: String) => void
}

export const Item: React.FC<PropsType> = props => {
    const { name } = props

    const onDelete = () => props.onDelete(name)
    const onSelect = () => props.onSelect(name)

    return (
        <div onClick={ onSelect }>
            <span>{ name }</span>
            <button onClick={ onDelete }>delete</button>
        </div>
    )
}
