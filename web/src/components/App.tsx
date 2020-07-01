import React from 'react';
import { LayerContextProvider } from './context/LayerContext';
import TrailMap from './TrailMap';

const App: React.FC = () => {
    return (
        <LayerContextProvider>
            <TrailMap />
        </LayerContextProvider>
    );
};

export default App;
