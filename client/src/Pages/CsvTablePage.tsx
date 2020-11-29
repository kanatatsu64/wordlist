import React from 'react'
import { useHistory, useParams } from 'react-router-dom'

import { CSV, getByName } from 'Api/Csv'
import { Card } from 'Api/Types'

type PropsType = { }

export const CsvTablePage: React.FC<PropsType> = props => {
    const { name } = useParams()

    const history = useHistory()
    const [csv, setCsv] = React.useState<CSV>(undefined)

    React.useEffect(() => {
        (async () => {
            setCsv(await getByName(name))
        })()
    }, [name])

    const onStartLearning = () => {
        history.push(`/csv/learn/${ name }`)
    }

    const viewCards = (cards: Card[]) => (
        <table>
            <tbody>
            { cards.map(viewCard) }
            </tbody>
        </table>
    )

    const viewCard = (card: Card) => {
        const { word, meaning, cardid } = card
        return (
            <tr key={ cardid }>
                <td>{ word }</td>
                <td>{ meaning }</td>
            </tr>
        )
    }

    const loading = (
        <div>
            <span>loading ...</span>
        </div>
    )

    return (
        <div>
            <h1>{ name }</h1>
            <button onClick={ onStartLearning }>Start Learning</button>
            { !!csv ? viewCards(csv.cards) : loading }
        </div>
    )
}
