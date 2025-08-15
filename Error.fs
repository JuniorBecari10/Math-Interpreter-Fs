module Error

type Error =
    {
        message: string
        pos: int
    }

let error msg pos =
    {
        message = msg
        pos = pos
    }
