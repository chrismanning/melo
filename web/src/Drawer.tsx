import React, {MutableRefObject} from 'react';
import AppBar from '@material-ui/core/AppBar';
import CssBaseline from '@material-ui/core/CssBaseline';
import Divider from '@material-ui/core/Divider';
import MaterialDrawer from '@material-ui/core/Drawer';
import Hidden from '@material-ui/core/Hidden';
import IconButton from '@material-ui/core/IconButton';
import InboxIcon from '@material-ui/icons/MoveToInbox';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemIcon from '@material-ui/core/ListItemIcon';
import ListItemText from '@material-ui/core/ListItemText';
import MailIcon from '@material-ui/icons/Mail';
import MenuIcon from '@material-ui/icons/Menu';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import {makeStyles, useTheme, Theme, createStyles} from '@material-ui/core/styles';
import {Link as RouterLink} from 'react-router-dom';
import {useQuery} from '@apollo/react-hooks';
import {gql} from "apollo-boost";
import * as API from "./API";
import CollectionBrowser, {CollectionBrowserRoute} from "./CollectionBrowser";
import {Route} from "react-router";
import {LinkProps} from "@material-ui/core/Link";
import {CircularProgress} from "@material-ui/core";
import {Alert} from "@material-ui/lab";

export const drawerWidth = 240;

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    root: {
      display: 'block',
    },
    drawer: {
      [theme.breakpoints.up('sm')]: {
        width: drawerWidth,
        flexShrink: 0,
      },
    },
    drawerPaper: {
      width: drawerWidth,
    },
    nested: {
      // paddingLeft: theme.spacing(4),
      width: drawerWidth - theme.spacing(2)
    },
    toolbar: theme.mixins.toolbar,
    unselectable: {
      "user-select": "none",
      "-webkit-user-select": "none",
      "-moz-user-select": "none",
    },
  }),
);

export default function Drawer() {
  const classes = useStyles();
  const theme = useTheme();

  const drawer = (
    <div className={classes.unselectable}>
      <Divider/>
      <List className={classes.nested}>
        <ListItem key="collections-list-heading">
          <ListItemText primary="Collections"/>
        </ListItem>
        <ListItem key="collections-list" >
          <List className={classes.nested}>
            <CollectionRouter/>
          </List>
        </ListItem>
      </List>
      <Divider/>
      <List>
        <ListItem key="settings menu item">
          <ListItemText primary="Settings"/>
        </ListItem>
      </List>
    </div>
  );

  // @ts-ignore
  const container = window !== undefined ? () => window.document.body : undefined;

  return (
    <div className={classes.root}>
      <nav className={classes.drawer} aria-label="main menu list">
        <MaterialDrawer
          container={container}
          variant="permanent"
          anchor={theme.direction === 'rtl' ? 'right' : 'left'}
          classes={{
            paper: classes.drawerPaper,
          }}
          ModalProps={{
            keepMounted: true, // Better open performance on mobile.
          }}
        >
          <div className={classes.toolbar} />
          {drawer}
        </MaterialDrawer>
      </nav>
    </div>
  );
}

const GET_COLLECTIONS = gql`
    query GetCollections {
        library {
            collections {
                id
                name
            }
        }
    }
`

export function CollectionRouter() {
  const classes = useStyles();
  let {loading, error, data, refetch} = useQuery<API.Data>(GET_COLLECTIONS)

  return <div>
    {loading && (
      <ListItem key="collections-loading">
        <CircularProgress/>
      </ListItem>
    )}
    {error && (
      <ListItem key="collections-error">
        <Alert severity="error">Error :(</Alert>
      </ListItem>
    )}
    {data?.library?.collections?.map(collection => (
      <>
        <ListItemLink to={"/collection/" + collection.id} text={collection.name} className={classes.nested}/>
      </>
    ))}
    <Route/>
  </div>
}

interface ListItemLinkProps extends LinkProps {
  to: string,
  text: string
}

function ListItemLink(props: Omit<ListItemLinkProps, 'ref'>) {
  const {to, text, ...other} = props;

  return (
    <li>
      <ListItem button component={RouterLink} to={to} {...other}>
        <ListItemText primary={text}/>
      </ListItem>
    </li>
  );
}
