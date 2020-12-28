import React, {MouseEventHandler} from 'react';
import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import IconButton from '@material-ui/core/IconButton';
import Typography from '@material-ui/core/Typography';
import InputBase from '@material-ui/core/InputBase';
import { createStyles, fade, Theme, makeStyles } from '@material-ui/core/styles';
import MenuIcon from '@material-ui/icons/Menu';
import SearchIcon from '@material-ui/icons/Search';
import {useScrollTrigger} from "@material-ui/core";
import {Link as RouterLink} from 'react-router-dom';
import Link from '@material-ui/core/Link';

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    root: {
      flexGrow: 1,
    },
    menuButton: {
      marginRight: theme.spacing(2),
      [theme.breakpoints.up('sm')]: {
        display: 'none',
      },
    },
    title: {
      flexGrow: 1,
      color: theme.palette.common.white,
      // color: 'white',
      display: 'none',
      [theme.breakpoints.up('sm')]: {
        display: 'block',
      },
    },
    search: {
      position: 'relative',
      borderRadius: theme.shape.borderRadius,
      backgroundColor: fade(theme.palette.common.white, 0.15),
      '&:hover': {
        backgroundColor: fade(theme.palette.common.white, 0.25),
      },
      marginLeft: 0,
      width: '100%',
      [theme.breakpoints.up('sm')]: {
        marginLeft: theme.spacing(1),
        width: 'auto',
      },
    },
    searchIcon: {
      padding: theme.spacing(0, 2),
      height: '100%',
      position: 'absolute',
      pointerEvents: 'none',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
    },
    inputRoot: {
      color: 'inherit',
    },
    inputInput: {
      padding: theme.spacing(1, 1, 1, 0),
      // vertical padding + font size from searchIcon
      paddingLeft: `calc(1em + ${theme.spacing(4)}px)`,
      transition: theme.transitions.create('width'),
      width: '100%',
      [theme.breakpoints.up('sm')]: {
        width: '12ch',
        '&:focus': {
          width: '20ch',
        },
      },
    },
    offset: theme.mixins.toolbar,
  }),
);

function ElevationScroll(props: { children: React.ReactElement }) {
  const { children } = props;
  const trigger = useScrollTrigger({
    disableHysteresis: true,
    threshold: 0,
  });

  return React.cloneElement(children, {
    elevation: trigger ? 4 : 0,
  });
}

export default function SearchAppBar() {
  const classes = useStyles();

  return (
    <>
      <ElevationScroll>
        <AppBar className={classes.root} position="sticky" >
          <Toolbar>
            {/*<IconButton*/}
            {/*  edge="start"*/}
            {/*  className={classes.menuButton}*/}
            {/*  color="inherit"*/}
            {/*  aria-label="open drawer"*/}
            {/*  onClick={props.onMenuClick}*/}
            {/*>*/}
            {/*  <MenuIcon />*/}
            {/*</IconButton>*/}

            <Link variant="h6" className={classes.title} to="/" component={RouterLink as any}>Melo</Link>
            {/*<Typography className={classes.title} variant="h6" noWrap>*/}
            {/*  Melo*/}
            {/*</Typography>*/}
            {/*<div className={classes.search}>*/}
            {/*  <div className={classes.searchIcon}>*/}
            {/*    <SearchIcon />*/}
            {/*  </div>*/}
            {/*  <InputBase*/}
            {/*    placeholder="Search…"*/}
            {/*    classes={{*/}
            {/*      root: classes.inputRoot,*/}
            {/*      input: classes.inputInput,*/}
            {/*    }}*/}
            {/*    inputProps={{ 'aria-label': 'search' }}*/}
            {/*  />*/}
            {/*</div>*/}
          </Toolbar>
        </AppBar>
      </ElevationScroll>
    </>
  );
}
