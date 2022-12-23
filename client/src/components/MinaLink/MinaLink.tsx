import { ReactNode } from "react"

interface MinaLinkProps {
    href: string,
    spaces: boolean | undefined,
    underline: boolean | undefined,
    children: ReactNode
}

export const MinaLink: React.FC<MinaLinkProps> = ({ href, spaces, underline, children }) => {
    return (
        <>
            {spaces && <>&nbsp;</>}
            <a
                href={href}
                className="text-OrangeMINA hover:opacity-80 transition-all duration-200"
            >
                {underline ? <u>{children}</u> : children}
            </a>
            {spaces && <>&nbsp;</>}
        </>
    )
}