import React from 'react';
import useAddMarker from './hooks/useAddMarker';

const AddMarkerButton: React.FC = () => {
    const { setActivate, activate } = useAddMarker(false);

    return <button onClick={() => setActivate(!activate)}>Add Points</button>;
};

export default AddMarkerButton;
