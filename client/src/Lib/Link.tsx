import React from 'react'

type PropsType = {
    to : string
}

export const Link : React.FC<PropsType> = props => (
    <a href={ props.to }>{ props.children }</a>
)
