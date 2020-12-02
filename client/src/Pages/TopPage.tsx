import React from 'react'
import { useHistory } from 'react-router-dom'

import { getNameList } from 'Api/Bundle'
import { Item } from 'Bundle/Item'
import { FCList } from 'Lib/FCList'
import { ListItem } from 'Lib/ListItem'

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
        history.push(`/bundle/table/${ name }`)
    }
    const onDelete = name => {
        alert(`delete ${ name }`)
    }

    const getItem = (name: string) => (
        <ListItem>
            <Item name={ name }
                  onSelect={ onSelect }
                  onDelete={ onDelete }
            ></Item>
        </ListItem>
    )
    const getKey = (name: string) => name

    return (
        <div>
            <FCList values={ csvNames } itemGen={ getItem } keyGen={ getKey }></FCList>
        </div>
    )
}
