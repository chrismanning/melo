import React from 'react';
import './App.css';
import SourcesList from './SourcesList'
import SearchAppBar from "./SearchAppBar"

export default function App() {
  return <div className="App">
    <SearchAppBar/>
    <SourcesList/>
  </div>
}
