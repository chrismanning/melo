import React, {createRef, useRef} from 'react';
import {makeStyles, Theme, createStyles} from '@material-ui/core/styles';
import List from '@material-ui/core/List';
import Link, {LinkProps} from '@material-ui/core/Link';
import ListItem from '@material-ui/core/ListItem';
import Collapse from '@material-ui/core/Collapse';
import ListItemText from '@material-ui/core/ListItemText';
import Typography from '@material-ui/core/Typography';
import ExpandLess from '@material-ui/icons/ExpandLess';
import ExpandMore from '@material-ui/icons/ExpandMore';
import Breadcrumbs from '@material-ui/core/Breadcrumbs';
import {Route, Switch} from 'react-router';
import {Link as RouterLink, BrowserRouter} from 'react-router-dom';
import {Omit} from '@material-ui/types';
import './App.css';
import SearchAppBar from "./SearchAppBar"
import Drawer, {drawerWidth} from "./Drawer";
import CollectionBrowser, {CollectionBrowserRoute} from "./CollectionBrowser";
import Box from '@material-ui/core/Box';

export default function App() {
  const classes = useStyles();
  return <div className="App">
    <BrowserRouter>
      <SearchAppBar/>
      <Drawer/>
      <Switch>
        <Box className={classes.mainContent}>
          <Route path={"/collection/:collectionId"} render={({match}) => (
            <CollectionBrowser collectionId={match.params.collectionId}/>
          )}/>
        </Box>
        <Route path="/"/>
      </Switch>
    </BrowserRouter>
  </div>
}

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    mainContent: {
      [theme.breakpoints.up('sm')]: {
        paddingLeft: drawerWidth,
      },
    }
  })
)
