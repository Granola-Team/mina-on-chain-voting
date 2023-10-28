import './styles.css'

function LoadingDots() {
  return (
    <div className="flex justify-center items-center min-h-screen">
        <div className="space-x-2">
          <span className='load-balls'></span>
       </div>
    </div>
  );
}

export default LoadingDots;
