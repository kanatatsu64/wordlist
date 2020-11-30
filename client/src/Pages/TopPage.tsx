import React from 'react'
import { useHistory } from 'react-router-dom'

import { getNameList } from 'Api/Csv'
import { Item } from 'Csv/Item'
import { FCList } from 'Lib/FCList'

type PropsType = { }

export const TopPage: React.FC<PropsType> = props => {
    const history = useHistory()
    const [csvNames, setCsvNames] = React.useState<string[]>([])

    React.useEffect(() => {
        (async () => {
            setCsvNames(await getNameList())
        })()
    }, [])

    const onSelect = name => {
        history.push(`/csv/table/${ name }`)
    }
    const onDelete = name => {
        alert(`delete ${ name }`)
    }

    const getItem = (name: string) => (
        <Item name={ name }
                onSelect={ onSelect }
                onDelete={ onDelete }
        ></Item>
    )
    const getKey = (name: string) => name

    return (
        <div>
            <h1>Wordlist</h1>
            <FCList values={ csvNames } itemGen={ getItem } keyGen={ getKey }></FCList>
        </div>
    )
}
