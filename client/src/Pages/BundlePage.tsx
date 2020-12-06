import React, { ReactElement } from 'react'
import { useHistory, useParams } from 'react-router-dom'
import Modal from 'react-modal'

import { BundleID, Bundle, Card } from 'Types'
import { load } from 'Api/Bundle'
import { Center } from 'Lib/Align'
import { Sequence } from 'Lib/Sequence'
import { Dial } from 'Lib/Dial'
import { Loading } from 'Lib/Loading'
import { mod } from 'Utils'
import { UploadForm } from 'Bundle/UploadForm'

type PropsType = {
    setMenu: (menu: ReactElement) => void
}
type ParamsType = {
    bundleId: BundleID
}

export const BundlePage: React.FC<PropsType> = props => {
    const { bundleId } = useParams<ParamsType>()

    const history = useHistory()
    const [bundle, setBundle] = React.useState<Bundle>(undefined)
    const [from, setFrom] = React.useState(0)
    const [isOpen, setIsOpen] = React.useState(false)
    const cards = React.useMemo(() => bundle?.cards, [bundle])

    const onStartLearning = () => {
        history.push(`/bundle/learn/${ bundleId }`)
    }

    const openModal = () => {
        setIsOpen(true)
    }
    const closeModal = () => {
        setIsOpen(false)
    }

    const menu = React.useMemo(() => (
        <>
            <button onClick={ onStartLearning }>Start Learning</button>
            <button onClick={ openModal }>Upload CSV</button>
        </>
    ), [])

    const updateBundle = async () => {
        setBundle(await load(bundleId))
    }

    React.useEffect(() => {
        updateBundle()
    }, [bundleId])
    React.useEffect(() => {
        props.setMenu(menu)
    }, [menu])

    const onUploaded = async () => {
        closeModal()
        updateBundle()
    }

    const onSelect = (card: Card) => {
        history.push(`/card/${card.cardid}`)
    }

    const onNext = () => {
        setFrom(mod(from + 1, cards.length))
    }

    const onPrev = () => {
        setFrom(mod(from - 1, cards.length))
    }

    const viewCards = (cards: Card[]) => (
        <Dial onNext={ onNext } onPrev={ onPrev }>
            <Center>
            <table>
                <tbody>
                    <Sequence from={ from } count={ 3 }>
                        {cards.map(card => {
                            const { word, meaning, cardid } = card
                            const onClick = () => onSelect(card)
                            return (
                                <tr key={ cardid } onClick={ onClick }>
                                    <td><Center>{ word }</Center></td>
                                    <td><Center>{ meaning }</Center></td>
                                </tr>
                            )
                        })}
                    </Sequence>
                </tbody>
            </table>
            </Center>
        </Dial>
    )

    return (
        !!bundle ? (
            <>
                <Center>
                    <h1>{ bundle.name }</h1>
                </Center>
                { viewCards(cards) }
                <Modal
                    isOpen={ isOpen }
                    onRequestClose={ closeModal }
                >
                    <UploadForm bundleId={ bundleId } onUploaded={ onUploaded }></UploadForm>
                </Modal>
            </>
        ): <Loading></Loading>
    )
}
