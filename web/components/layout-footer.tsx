import { siteConfig } from 'common/config';

export const Footer = () => {
  return (
    <footer className="py-6 md:px-8 md:py-0">
      <div className="container flex flex-col items-center justify-between gap-4 md:h-24 md:flex-row">
        <p className="text-center text-xs leading-loose text-muted-foreground md:text-left">
          Built with ❤️ by{' '}
          <a
            href={siteConfig.links.granola}
            target="_blank"
            rel="noreferrer"
            className="font-medium underline underline-offset-4 text-logoOrange"
          >
            Granola
          </a>
          .
        </p>
      </div>
    </footer>
  );
};
