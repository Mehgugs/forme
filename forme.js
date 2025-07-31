{
    const identifier = "__forme-0.0.1"


    const suppressor = e => e.preventDefault()

    const killNaN = x => {
        isNaN(x) ? null : x
    }

    if (!Object.hasOwnProperty.call(HTMLFormElement.prototype, `${identifier}__entries`)) {

        Object.defineProperties(HTMLFormElement.prototype, {

            [`${identifier}__entries`]: {
                get() {
                    
                    return [...new FormData(this)]
                        .map(([name, value]) => [name, value, killNaN(this.elements[name]?.valueAsNumber), this.elements[name]?.valueAsDate?.getTime?.()])
                }
            },

            [`${identifier}__messages`]: {
                get() {

                    const result = []

                    for (let input of this.elements) {

                        if (input instanceof RadioNodeList) {

                            const value = input.value 
                            input = [...input.values()].filter(e => e.value === value && e.checked)

                            if (!input) {

                                continue
                            }
                        }
                        if (input.validationMessage !== "") {

                            result.push([input.name ?? input.id, input.validationMessage])
                        }
                        
                    }

                    return result
                }
            },

            [`${identifier}__validity`]: {
                get() {

                    return this.matches(":valid") // literally every method creates events :/
                }
            },

            [`${identifier}__suppress`]: {
                set(v) {

                    if (v) {
                        this.removeEventListener("invalid", suppressor, true)
                        this.addEventListener("invalid", suppressor, true)
                    } else {

                        this.removeEventListener("invalid", suppressor, true)
                    }
                }
            },
        })
    }
}