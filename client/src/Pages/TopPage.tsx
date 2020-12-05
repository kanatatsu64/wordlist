import React, { ReactElement } from 'react'
import { useHistory } from 'react-router-dom'
import Modal from 'react-modal'

import { BundleInfo } from 'Types'
import { loadInfos } from 'Api/Bundle'
import { Item } from 'Bundle/Item'
import { FCList } from 'Lib/FCList'
import { ListItem } from 'Lib/ListItem'
import { Center } from 'Lib/Align'
import { CreateForm } from 'Bundle/CreateForm'

type PropsType = {
    setMenu: (menu: ReactElement) => void
}

export const TopPage: React.FC<PropsType> = props => {
    const history = useHistory()
    const [bundleInfos, setBundleInfos] = React.useState<BundleInfo[]>([])
    const [isOpen, setIsOpen] = React.useState(false)

    const openModal = () => {
        setIsOpen(true)
    }
    const closeModal = () => {
        setIsOpen(false)
    }

    const menu = React.useMemo(() => (
        <button onClick={ openModal }>Add Bundle</button>
    ), [])

    const updateBundleInfos = async () => {
        setBundleInfos(await loadInfos())
    }

    React.useEffect(() => {
        updateBundleInfos()
    }, [])
    React.useEffect(() => {
        props.setMenu(menu)
    }, [menu])

    const onSelect = (bundleInfo: BundleInfo) => {
        history.push(`/bundle/table/${ bundleInfo.bundleid }`)
    }
    const onDelete = (bundleInfo: BundleInfo) => {
        alert(`delete ${ bundleInfo.name }`)
    }
    const onCreate = () => {
        closeModal()
        updateBundleInfos()
    }

    const getItem = (bundleInfo: BundleInfo) => (
        <ListItem>
            <Item bundleInfo={ bundleInfo }
                  onSelect={ onSelect }
                  onDelete={ onDelete }
            ></Item>
        </ListItem>
    )
    const getKey = (bundleInfo: BundleInfo) => bundleInfo.bundleid

    return (
        <>
            <Center>
                <h1>Bundle List</h1>
            </Center>
            <FCList values={ bundleInfos } itemGen={ getItem } keyGen={ getKey }></FCList>
            <Modal
                isOpen={ isOpen }
                onRequestClose={ closeModal }
            >
                <CreateForm onCreate={ onCreate }></CreateForm>
            </Modal>
        </>
    )
}
