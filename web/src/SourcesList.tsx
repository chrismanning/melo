import React from 'react';
import {useQuery} from '@apollo/react-hooks';
import {gql} from "apollo-boost";
import useTheme from "@material-ui/core/styles/useTheme";
import useMediaQuery from "@material-ui/core/useMediaQuery";
import * as API from "./API";
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListSubheader from '@material-ui/core/ListSubheader';
import Collapse from '@material-ui/core/Collapse';
import Typography from '@material-ui/core/Typography';
import {makeStyles, Theme, createStyles, withStyles, WithStyles} from '@material-ui/core/styles';
import produce from "immer";
import clsx from 'clsx';
import Dialog from "@material-ui/core/Dialog";
import DialogContent from "@material-ui/core/DialogContent";
import SourceMetadataEditor from "./SourceMetadataEditor";
import MuiDialogTitle from "@material-ui/core/DialogTitle";
import IconButton from "@material-ui/core/IconButton";
import CloseIcon from "@material-ui/icons/Close";
import ExpandMoreIcon from "@material-ui/icons/ExpandMore";
import ExpandLessIcon from "@material-ui/icons/ExpandLess";
import EditIcon from "@material-ui/icons/Edit";
import DialogActions from "@material-ui/core/DialogActions";
import Button from "@material-ui/core/Button";
import {SourceItem} from "./API";
import {LinearProgress} from "@material-ui/core";
import {Alert} from "@material-ui/lab";
import ListItemSecondaryAction from "@material-ui/core/ListItemSecondaryAction";
import Paper from "@material-ui/core/Paper";
import Fab from "@material-ui/core/Fab";

const GET_SOURCES = gql`
    query GetSources {
        library {
            sourceGroups {
                groupParentUri
                groupTags {
                    albumArtist
                    albumTitle
                    date
                    discNumber
                }
                sources {
                    id
                    sourceName
                    sourceUri
                    format
                    downloadUri
                    length
                    metadata {
                        mappedTags {
                            albumArtist
                            artistName
                            trackTitle
                            trackNumber
                        }
                    }
                }
            }
        }
    }
`;

const styles = (theme: Theme) =>
  createStyles({
    root: {
      margin: 0,
      padding: theme.spacing(2),
    },
    closeButton: {
      position: 'absolute',
      right: theme.spacing(1),
      top: theme.spacing(1),
      color: theme.palette.grey[500],
    },
  });

interface DialogTitleProps extends WithStyles<typeof styles> {
  id: string;
  children: React.ReactNode;
  onClose: () => void;
}

const DialogTitle = withStyles(styles)((props: DialogTitleProps) => {
  const {children, classes, onClose, ...other} = props;
  return (
    <MuiDialogTitle disableTypography className={classes.root} {...other}>
      <Typography variant="h6">{children}</Typography>
      {onClose && (
        <IconButton aria-label="close" className={classes.closeButton} onClick={onClose}>
          <CloseIcon/>
        </IconButton>
      )}
    </MuiDialogTitle>
  );
});

export default function SourcesList() {
  const classes = useStyles();
  const {loading, error, data, refetch} = useQuery<API.Data>(GET_SOURCES);

  const [selectedIndices, setSelectedIndices] = React.useState(new Set() as Set<number>);
  const [lastSelected, setLastSelected] = React.useState(null as number | null);
  const [collapsedGroups, setCollapsedGroups] = React.useState(new Set() as Set<number>);
  const [selectedSrc, setSelectedSrc] = React.useState(null as SourceItem | null);
  const [selectedSrcs, setSelectedSrcs] = React.useState(new Set() as Set<SourceItem>);
  const [openMetadataEditor, setOpenMetadataEditor] = React.useState(false);
  const theme = useTheme();
  const fullScreen = useMediaQuery(theme.breakpoints.down('sm'));

  if (loading) return <LinearProgress/>;
  if (error) return <Alert severity="error">Error :(</Alert>;

  let sourceGroups: API.SourceGroup[] = data?.library?.sourceGroups || [];

  let i = 0;
  let items = sourceGroups.map((sourceGroup: API.SourceGroup) => {
    const handleListItemClick = (
      event: React.MouseEvent<HTMLElement, MouseEvent>,
      indices: number[],
      groupIndex: number
    ) => {
      event.persist();
      if(event.shiftKey) {
        const from = lastSelected || 0;
        setSelectedIndices(produce(selectedIndices => {
          indices.forEach(index => {
            if (from > index) {
              for (let i = index; i < from; i++) {
                selectedIndices.add(i);
              }
            } else {
              for (let i = from; i <= index; i++) {
                selectedIndices.add(i);
              }
            }
          })
        }));
        setSelectedSrcs(produce(selectedSrcs => {
          indices.forEach(index => {
            const sourceItem = sourceGroup.sources[index - groupIndex]
            if (from > index) {
              selectedSrcs.add(sourceItem);
            } else {
              for (let i = from; i <= index; i++) {
                selectedSrcs.add(sourceItem);
              }
            }
          })
        }));
      } else if(event.ctrlKey) {
        setSelectedIndices(produce(selectedIndices => {
          indices.forEach(index => {
            if (selectedIndices.has(index)) {
              selectedIndices.delete(index);
            } else {
              selectedIndices.add(index);
            }
          })
        }));
        setSelectedSrcs((produce(selectedSrcs => {
          indices.forEach(index => {
            const sourceItem = sourceGroup.sources[index - groupIndex]
            if (selectedSrcs.has(sourceItem)) {
              selectedSrcs.delete(sourceItem);
            } else {
              selectedSrcs.add(sourceItem);
            }
          })
        })));
      } else {
        setSelectedIndices(produce(selectedIndices => {
          selectedIndices.clear();
          indices.forEach(index => selectedIndices.add(index))
        }));
        setSelectedSrcs(produce(selectedSrcs => {
          selectedSrcs.clear()
          indices.forEach(index => {
            const sourceItem = sourceGroup.sources[index - groupIndex]
            selectedSrcs.add(sourceItem)
          })
        }));
      }
      if(indices && indices.length > 0) {
        setLastSelected(indices[0]);
        setSelectedSrc(sourceGroup.sources[indices[0] - groupIndex]);
      }
    };

    let groupIndex = i;
    const toggleCollapseGroup = () => {
      setCollapsedGroups(produce(collapsedGroups => {
        if (collapsedGroups.has(groupIndex)) {
          collapsedGroups.delete(groupIndex);
        } else {
          collapsedGroups.add(groupIndex);
        }
      }));
    };
    const sourceListItems = sourceGroup.sources.map((sourceItem) => {
      const mappedTags = sourceItem.metadata.mappedTags;

      let j = i++;
      return <ListItem key={`sourceGroup-${groupIndex}-source-${sourceItem.id}`} dense className={classes.track}
                       onClick={event => handleListItemClick(event, [j], groupIndex)}
                       selected={selectedIndices.has(j)}>
        <Typography className={classes.trackNumber}>{mappedTags.trackNumber}</Typography>
        <Typography>{mappedTags.trackTitle || sourceItem.sourceName}</Typography>
      </ListItem>
    })
    const end = i
    const handleSubheaderClick = (event: React.MouseEvent<HTMLElement, MouseEvent>) => {
      event.preventDefault()
      event.persist()
      const indices = Array.from({length: end - groupIndex}, (_, j) => j + groupIndex)
      handleListItemClick(event, indices, groupIndex)
    }
    return <>
      <ListSubheader component="div" key={`sourceGroup-${groupIndex}`} onClick={handleSubheaderClick}>
        <SourceListSubheader sourceGroup={sourceGroup} collapsed={collapsedGroups.has(groupIndex)} onToggleCollapse={toggleCollapseGroup}/>
      </ListSubheader>
      <Collapse in={!collapsedGroups.has(groupIndex)}>
        {sourceListItems}
      </Collapse>
    </>;
  });

  const handleClose = () => setOpenMetadataEditor(false)
  const handleSubmit = () => {
    setOpenMetadataEditor(false)
    refetch()
  }
  let selectedSrcIds = [...selectedSrcs].map(src => src.id)
  return <>
    <List subheader={<li/>} className={clsx(classes.root, classes.unselectable)}>
      {items}
    </List>
    <Fab className={classes.fab} aria-label="Edit Metadata"
         onClick={() => {
           if (selectedSrcs.size > 0) {
             setOpenMetadataEditor(true)
           }
         }}>
      <EditIcon/>
    </Fab>

    {selectedSrc && (
      <Dialog disablePortal={true} open={openMetadataEditor} onClose={handleClose}
        fullScreen={fullScreen} aria-labelledby="form-dialog-title">
        <DialogTitle id="form-dialog-title" onClose={handleClose}>
          <Typography>Edit Metadata</Typography>
        </DialogTitle>
        <DialogContent>
          <SourceMetadataEditor srcIds={selectedSrcIds} onSuccess={handleSubmit} onFailure={() => {}}/>
        </DialogContent>
        <DialogActions>
          <Button form="metadata-form" type="submit" autoFocus color="primary">
            Save changes
          </Button>
          <Button form="metadata-form" type="reset" onClick={handleClose} autoFocus color="secondary">
            Cancel
          </Button>
          <Button form="metadata-form" type="reset" autoFocus>
            Reset
          </Button>
        </DialogActions>
      </Dialog>
    )}
  </>
}

type SourceListSubheaderProps = {
  sourceGroup: API.SourceGroup,
  onToggleCollapse: () => void,
  collapsed: boolean,
}

function SourceListSubheader(props: SourceListSubheaderProps) {
  const classes = useStyles();
  const sourceGroup = props.sourceGroup;
  const albumArtist = sourceGroup.groupTags.albumArtist?.join(" / ") || "<unknown artist>";
  const albumTitle = sourceGroup.groupTags.albumTitle || "<unknown album>";
  // const groupUri = sourceGroup.groupParentUri || "<unknown uri>";
  const date = sourceGroup.groupTags.date || "<unknown date>";
  return <Paper className={classes.listSection} square>
    <Typography align="left">{albumArtist}</Typography>
    <Typography align="left">{date}</Typography>
    <Typography align="left">{albumTitle}</Typography>
    <IconButton className={classes.collapseButton} onClick={event => {event.preventDefault(); props.onToggleCollapse()}}>
      {props.collapsed && <ExpandMoreIcon />}
      {props.collapsed || <ExpandLessIcon />}
    </IconButton>
  </Paper>
    ;
}

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    root: {
      width: '100%',
      backgroundColor: theme.palette.background.paper,
    },
    heading: {
      fontSize: theme.typography.pxToRem(15),
    },
    secondaryHeading: {
      fontSize: theme.typography.pxToRem(15),
      color: theme.palette.text.secondary,
    },
    icon: {
      verticalAlign: 'bottom',
      height: 20,
      width: 20,
    },
    details: {
      alignItems: 'center',
    },
    column: {
      flexBasis: '33.33%',
    },
    helper: {
      borderLeft: `2px solid ${theme.palette.divider}`,
      padding: theme.spacing(1, 2),
    },
    link: {
      color: theme.palette.primary.main,
      textDecoration: 'none',
      '&:hover': {
        textDecoration: 'underline',
      },
    },
    listSection: {
      padding: theme.spacing(1),
    },
    ul: {
      backgroundColor: 'inherit',
      padding: 0,
    },
    track: {
      paddingLeft: theme.spacing(2)
    },
    trackNumber: {
      paddingRight: theme.spacing(2)
    },
    unselectable: {
      "user-select": "none",
      "-webkit-user-select": "none",
      "-moz-user-select": "none",
    },
    collapseButton: {
      position: 'absolute',
      zIndex: 1,
      bottom: 0,
      left: 0,
      right: 0,
      margin: '0 auto',
    },
    fab: {
      position: 'fixed',
      bottom: theme.spacing(2),
      right: theme.spacing(2),
      zIndex: 100,
    },
  }),
);
