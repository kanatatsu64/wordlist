import React from 'react'

type PropsType = { }

export const Row : React.FC<PropsType> = props => (
    <div> { props.children } </div>
)
