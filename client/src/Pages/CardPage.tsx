import React, { ReactElement } from 'react'
import { useHistory, useParams } from 'react-router-dom'
import Modal from 'react-modal'

import { CardID, Card } from 'Types'
import { Loading } from 'Lib/Loading'
import { View } from 'Card/View'
import { Edit } from 'Card/Edit'
import { load, delete_ } from 'Api/Card'

type PropsType = {
    setMenu: (menu: ReactElement) => void
}
type ParamsType = {
    cardId: CardID
}

export const CardPage: React.FC<PropsType> = props => {
    const history = useHistory()
    const { cardId } = useParams<ParamsType>()

    const [card, setCard] = React.useState<Card>(undefined)
    const [isEditOpen, setIsEditOpen] = React.useState(false)
    const [isDeleteOpen, setIsDeleteOpen] = React.useState(false)

    const openEditModal = () => {
        setIsEditOpen(true)
    }
    const closeEditModal = () => {
        setIsEditOpen(false)
    }

    const openDeleteModal = () => {
        setIsDeleteOpen(true)
    }
    const closeDeleteModal = () => {
        setIsDeleteOpen(false)
    }

    const menu = React.useMemo(() => (
        <>
            <button onClick={ openEditModal }>Edit</button>
            <button onClick={ openDeleteModal }>Delete</button>
        </>
    ), [])

    const updateCard = async () => {
        setCard(await load(cardId))
    }

    React.useEffect(() => {
        updateCard()
    }, [cardId])
    React.useEffect(() => {
        props.setMenu(menu)
    }, [menu])

    const onUpdated = async () => {
        await updateCard()
        closeEditModal()
    }

    const onDelete = () => {
        history.goBack()
        delete_(cardId)
    }

    return (
        !!card ? (
            <>
                <View card={ card }></View>
                <Modal
                    isOpen={ isEditOpen }
                    onRequestClose={ closeEditModal }
                >
                    <Edit card={ card } onUpdated={ onUpdated }></Edit>
                </Modal>
                <Modal
                    isOpen={ isDeleteOpen }
                    onRequestClose={ closeDeleteModal }
                >
                    <div>
                        Are you sure to delete the card?
                    </div>
                    <div>
                        <button onClick={ onDelete }>Yes</button>
                        <button onClick={ closeDeleteModal }>Not</button>
                    </div>
                </Modal>
            </>
        ) : <Loading></Loading>
    )
}
