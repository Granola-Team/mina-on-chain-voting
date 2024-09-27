import './styles.css'

export default function LoadingDots() {
    return (
      <div className="flex justify-center items-center min-h-screen">
        <div className="space-x-2">
          <span className='load-dots w-12 h-12 block mx-auto relative text-yellow-500 box-border animate-rotation'></span>
        </div>
      </div>
    );
  }