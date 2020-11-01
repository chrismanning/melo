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
import DialogActions from "@material-ui/core/DialogActions";
import Button from "@material-ui/core/Button";
import {SourceItem} from "./API";

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
      {onClose ? (
        <IconButton aria-label="close" className={classes.closeButton} onClick={onClose}>
          <CloseIcon/>
        </IconButton>
      ) : null}
    </MuiDialogTitle>
  );
});

export default function SourcesList() {
  const classes = useStyles();
  const {loading, error, data, refetch} = useQuery<API.Data>(GET_SOURCES);

  const [selectedIndexes, setSelectedIndexes] = React.useState(new Set() as Set<number>);
  const [lastSelected, setLastSelected] = React.useState(null as number | null);
  const [collapsedGroups, setCollapsedGroups] = React.useState(new Set() as Set<number>);
  const [selectedSrc, setSelectedSrc] = React.useState(null as SourceItem | null);
  const [openMetadataEditor, setOpenMetadataEditor] = React.useState(false);
  const theme = useTheme();
  const fullScreen = useMediaQuery(theme.breakpoints.down('sm'));

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error :(</p>;

  let sourceGroups: API.SourceGroup[] = data?.library?.sourceGroups || [];

  let i = 0;
  let items = sourceGroups.map((sourceGroup: API.SourceGroup) => {
    const handleListItemClick = (
      event: React.MouseEvent<HTMLElement, MouseEvent>,
      index: number,
      sourcesIndex: number
    ) => {
      event.persist();
      setLastSelected(index);
      setSelectedSrc(sourceGroup.sources[sourcesIndex]);
      setSelectedIndexes(produce(indexes => {
        if (event.shiftKey) {
          const from = lastSelected || 0;
          if (from > index) {
            for (let i = index; i < from; i++) {
              indexes.add(i);
            }
          } else {
            for (let i = from; i < index; i++) {
              indexes.add(i);
            }
          }
        } else if (event.ctrlKey) {
          if (indexes.has(index)) {
            indexes.delete(index);
          } else {
            indexes.add(index);
          }
        } else {
          indexes.clear();

          indexes.add(index);
        }
      }));
    };

    let groupIndex = i;
    const handleSubheaderDoubleClick = () => {
      setCollapsedGroups(produce(collapsedGroups => {
        if (collapsedGroups.has(groupIndex)) {
          collapsedGroups.delete(groupIndex);
        } else {
          collapsedGroups.add(groupIndex);
        }
      }));
    };
    return <>
      <ListSubheader key={`sourceGroup-${groupIndex}`} onDoubleClick={handleSubheaderDoubleClick}>
        <SourceListSubheader sourceGroup={sourceGroup}/>
      </ListSubheader>
      <Collapse in={!collapsedGroups.has(groupIndex)}>
        {sourceGroup.sources.map((sourceItem) => {
          const mappedTags = sourceItem.metadata.mappedTags;

          let j = i++;
          return <ListItem key={`sourceGroup-${groupIndex}-source-${sourceItem.id}`} dense className={classes.track}
                           onClick={event => handleListItemClick(event, j, j - groupIndex)}
                           onDoubleClick={_ => setOpenMetadataEditor(true)} selected={selectedIndexes.has(j)}>
            <Typography className={classes.trackNumber}>{mappedTags.trackNumber}</Typography>
            <Typography>{mappedTags.trackTitle || sourceItem.sourceName}</Typography>
          </ListItem>
        })}
      </Collapse>
    </>;
  });

  const handleClose = () => setOpenMetadataEditor(false)
  const handleSubmit = () => {
    setOpenMetadataEditor(false)
    refetch()
  }
  return <>
    <List subheader={<li/>} className={clsx(classes.root, classes.unselectable)}>
      {items}
    </List>

    {selectedSrc && (
      <Dialog hidden={!selectedSrc} disablePortal={true} open={openMetadataEditor} onClose={handleClose}
        fullScreen={fullScreen} aria-labelledby="form-dialog-title">
        <DialogTitle id="form-dialog-title" onClose={handleClose}>Edit Metadata{selectedSrc && (` \u2014 ${selectedSrc.sourceName}`)}</DialogTitle>
        <DialogContent>
          <SourceMetadataEditor srcId={selectedSrc.id} onSuccess={handleSubmit} onFailure={() => {}}/>
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

function SourceListSubheader(props: { sourceGroup: API.SourceGroup }) {
  const classes = useStyles();
  const sourceGroup = props.sourceGroup;
  const albumArtist = sourceGroup.groupTags.albumArtist?.join(" / ") || "<unknown artist>";
  const albumTitle = sourceGroup.groupTags.albumTitle || "<unknown album>";
  const groupUri = sourceGroup.groupParentUri || "<unknown uri>";
  const date = sourceGroup.groupTags.date || "<unknown date>";
  return <div className={classes.listSection}>
    <Typography align="left">{albumArtist}</Typography>
    <Typography align="left">{date}</Typography>
    <Typography align="left">{albumTitle}</Typography>
  </div>
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
      // backgroundColor: ,
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
    }
  }),
);
