export type Language =
    'Japanese' |
    'English' |
    'Chinese' |
    'French' |
    'German'

export type Card = {
    cardid: string,
    language: Language,
    word: string,
    meaning: string
}
