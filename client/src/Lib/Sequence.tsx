import React, { ReactElement } from 'react'

type PropsType = {
    from?: number
    count?: number
    children: ReactElement[]
}

export const Sequence: React.FC<PropsType> = props => {
    const from = props.from || 0
    const count = props.count || props.children.length

    const items = React.useMemo(() => (
        props.children.take(from, count)
    ), [from, props.children])

    return (
        <>
            { items }
        </>
    )
}
