{
    const identifier = "__forme-0.0.1"


    const suppressor = e => e.preventDefault()

    if (!Object.hasOwnProperty.call(HTMLFormElement.prototype, `${identifier}__entries`)) {

        Object.defineProperties(HTMLFormElement.prototype, {

            [`${identifier}__entries`]: {
                get() {

                    const result = {}

                    for (const [key, value] of new FormData(this)) {

                        let element = this.elements[key]

                        if (element instanceof RadioNodeList) {

                            element = Array.from(element)
                                .find(e => e.name === key && e.value === value && e.checked)
                        }

                        if ((["date", "number"].includes(element?.type)) && (value === "")) {

                            continue
                        }

                        const message = element?.validationMessage ?? ""

                        if (!result.hasOwnProperty(key)) {

                            const entry = { 
                                value, 
                                label: element?.labels?.[0]?.textContent ?? key, 
                                message, 
                                id: element.id ?? '',
                            }

                            result[key] = entry
                        } else {

                            if (Array.isArray(result[key].value)) {

                                result[key].value.push(value)
                            } else {

                                result[key].value = [result[key].value, value]
                            }
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