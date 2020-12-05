export const cons = (classNames: string[]) => classNames.filter(x => !!x).join(' ')

export const mod = (value: number, modulo: number) => {
    if (modulo == 0) return 0
    const remainder = value % modulo
    if (remainder >= 0) return remainder
    else return (remainder + modulo)
}

declare global {
    interface Array<T> {
        take(from: number, count: number): Array<T>
    }
}

Array.prototype.take = function(from, count) {
    count = Math.min(count, this.length)
    let index = from
    const result = []
    while (count > 0) {
        result.push((this as any)[index])
        index = (index + 1) % (this as any).length
        count -= 1
    }
    return result
}
