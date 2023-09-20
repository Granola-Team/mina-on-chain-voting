import Balance from 'react-wrap-balancer';

import { cn } from 'common/utils';

function PageHeader({ className, children, ...props }: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <section className={cn('flex max-w-[980px] flex-col items-start gap-2 px-4 pt-6 md:pt-10', className)} {...props}>
      {children}
    </section>
  );
}

function PageHeaderHeading({ className, ...props }: React.HTMLAttributes<HTMLHeadingElement>) {
  return (
    <h1
      className={cn('text-3xl font-bold leading-tight tracking-tighter md:text-4xl lg:leading-[1.1]', className)}
      {...props}
    />
  );
}

function PageHeaderDescription({ className, ...props }: React.HTMLAttributes<HTMLParagraphElement>) {
  return <Balance className={cn('max-w-[750px] text-lg text-muted-foreground sm:text-lg', className)} {...props} />;
}

function SmallerPageHeaderDescription({ className, ...props }: React.HTMLAttributes<HTMLParagraphElement>) {
  return <Balance className={cn('max-w-[750px] text-base text-muted-foreground sm:text-base', className)} {...props} />;
}

export { PageHeader, PageHeaderHeading, PageHeaderDescription, SmallerPageHeaderDescription };
