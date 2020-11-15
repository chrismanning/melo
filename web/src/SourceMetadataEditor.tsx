import React, {FormEvent, RefObject, useState} from 'react';
import {useQuery, useMutation} from '@apollo/react-hooks';
import {gql} from "apollo-boost";
import * as API from "./API";
import Typography from "@material-ui/core/Typography";
import {createStyles, makeStyles, Theme} from '@material-ui/core/styles';
import Tab from '@material-ui/core/Tab';
import Alert from '@material-ui/lab/Alert';
import AlertTitle from '@material-ui/lab/AlertTitle';
import Tabs from '@material-ui/core/Tabs';
import 'react-data-grid/dist/react-data-grid.css'
import TextField, {TextFieldProps} from "@material-ui/core/TextField";

import LinearProgress from "@material-ui/core/LinearProgress";
import produce from "immer";
import MaterialGrid from "@material-ui/core/Grid";
import Accordian from "@material-ui/core/Accordion";
import AccordionActions from "@material-ui/core/AccordionActions";
import AccordionDetails from "@material-ui/core/AccordionDetails";
import AccordionSummary from "@material-ui/core/AccordionSummary";
import ExpandMoreIcon from "@material-ui/icons/ExpandMore";
import {ChangeSet, EditingCell, EditingState} from '@devexpress/dx-react-grid';
import {
  Grid,
  Table,
  TableHeaderRow,
  TableEditColumn,
  TableInlineCellEditing,
} from '@devexpress/dx-react-grid-material-ui';
import {MetadataPair} from "./API";
import IconButton from "@material-ui/core/IconButton";
import AddIcon from "@material-ui/icons/Add"
import DeleteIcon from "@material-ui/icons/Delete"

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    root: {
      '& .MuiTextField-root': {
        margin: theme.spacing(1),
        width: '25ch',
      },
    },
    tabbar: {
      marginBottom: 20,
    },
    placeholder: {
      height: 40,
    },
    editor: {
      flexGrow: 1,
      paddingBottom: theme.spacing(1),
    },
    doubleWidth: {
      '& .MuiTextField-root': {
        width: '50ch',
      },
    }
  }),
);

const SAVE_MAPPED_TAGS = gql`
    mutation UpdateSourcesByMappedTags($updates: [SourceUpdate!]!) {
        library {
            updateSources(updates: $updates) {
                results {
                    __typename
                    ... on UpdatedSource {
                        id
                    }
                    ... on FailedSourceUpdate {
                        id
                        msg
                    }
                }
            }
        }
    }
`

interface EditorProps<T> {
  onSuccess: () => void,
  onFailure: () => void,
  data: T,
}

function BasicEditor(props: EditorProps<[API.SourceItem]>) {
  let classes = useStyles();
  const [sources, setSources] = useState(props.data as [API.EditableSourceItem]);
  const [saveMappedTags, { error, loading }] = useMutation(SAVE_MAPPED_TAGS)

  let forms = sources.sort((a, b) => {
    return a.sourceUri.localeCompare(b.sourceUri)
  }).map(sourceItem => {
    let ref = React.createRef();
    return [ref, sourceItem, (<BasicEditorForm data={sourceItem} ref={ref} />)]
  })

  const handleReset = (event: FormEvent) => {
    event.preventDefault()
    setSources(_ => props.data as [API.EditableSourceItem])
    console.log("reset form")
    console.log("reset sources: " + JSON.stringify([...sources.values()]))
    forms.forEach(([ref]) => {
      (ref as RefObject<any>).current?.reset()
    })
  }

  const handleSubmit = (event: FormEvent) => {
    event.preventDefault()
    console.log("submitting form")
    console.log("getting updated values from child forms")
    let sources = forms.map(([ref, a, b]) => {
      return (ref as RefObject<any>).current?.submit() as API.EditableSourceItem
    })
    console.log("sources: " + JSON.stringify(sources))
    saveMappedTags( {
      variables: {
        updates: sources.map(source => {
          return {
            id: source.id,
            updateTags: {
              setMappedTags: source.metadata.mappedTags
            }
          }
        })
      }
    }).then(_ => {
      console.log("mapped tags saved")
      if (props.onSuccess) {
        props.onSuccess()
      }
    }, () => {
      if (error) {
        console.log("failed to save mapped tags: " + error.message)
        console.log("mappedTags: " + JSON.stringify(sources))
        console.log("errors: " + JSON.stringify(error?.graphQLErrors))
      }
      if (props.onFailure) {
        props.onFailure()
      }
    })
  }

  return (
    <div
      role="tabpanel"
      id="basic-editor"
      aria-labelledby="basic-editor"
      className={classes.editor}
    >
      {loading && (
        <LinearProgress/>
      )}
      {error && (
        <Alert severity="error">
          <AlertTitle>Failed to save tags</AlertTitle>
          {error.message}
        </Alert>
      )}
      <form id="metadata-form" onSubmit={handleSubmit} onReset={handleReset} noValidate autoComplete="off">
        {forms.map(([ref, src, form], i) => {
          const source = src as API.EditableSourceItem
          return <Accordian key={source?.id} defaultExpanded={i === 0}>
            <AccordionSummary expandIcon={<ExpandMoreIcon />}>
              <Typography>{source?.sourceName}</Typography>
            </AccordionSummary>
            <AccordionDetails>
              {form}
            </AccordionDetails>
          </Accordian>
        })}
      </form>
    </div>
  );
}

type BasicEditorFormProps = {
  data: API.EditableSourceItem
}

const BasicEditorForm = React.forwardRef((props: BasicEditorFormProps, ref) => {
  const [source, setSource] = useState(props.data)
  React.useImperativeHandle(ref, () => ({
    reset() {
      setSource(props.data)
    },
    submit() {
      return source
    }
  }))

  const handleChange = (event: React.ChangeEvent<HTMLInputElement|HTMLTextAreaElement>) => {
    console.log("handleChange: " + event.target.value)
    event.persist()
    setSource(produce(source => {
      let mappedTags = source?.metadata?.mappedTags;
      if (event?.target && source && mappedTags) {
        if (event.target.name === 'artistName'
          || event.target.name === 'albumArtist'
          || event.target.name === 'genre'
          || event.target.name === 'musicbrainzArtistId'
          || event.target.name === 'musicbrainzAlbumArtistId') {
          mappedTags[event.target.name as string] = event.target.value.split('\n')
        } else {
          mappedTags[event.target.name] = event.target.value;
        }
      }
    }))
  }

  let mappedTags = source.metadata.mappedTags
  return (
    <MaterialGrid
      container
      spacing={1}
      justify="flex-start"
      alignItems="stretch"
    >
      <MaterialGrid container item xs={12} spacing={1}>
        <MaterialGrid item xs={12} sm={6}>
          <MultilineTextField name={'artistName'} label="Artist Name" value={mappedTags.artistName || []}
                              onChange={handleChange}/>
        </MaterialGrid>
        <MaterialGrid item xs={12} sm={6}>
          <TextField name={'trackTitle'} label="Track Title" value={mappedTags.trackTitle || ""}
                     onChange={handleChange}/>
        </MaterialGrid>
      </MaterialGrid>
      <MaterialGrid container item xs={12} spacing={1}>
        <MaterialGrid item xs={12} sm={6}>
          <TextField name={'albumTitle'} label="Album Title" value={mappedTags.albumTitle || ""}
                     onChange={handleChange}/>
        </MaterialGrid>
        <MaterialGrid item xs={12} sm={6}>
          <TextField name={'date'} label="Date" value={mappedTags.date || ""} onChange={handleChange}/>
        </MaterialGrid>
      </MaterialGrid>
      <MaterialGrid container item xs={12} spacing={1}>
        <MaterialGrid item xs={12} sm={6}>
          <MultilineTextField name={'genre'} label="Genre" value={mappedTags.genre || []} onChange={handleChange}/>
        </MaterialGrid>
        <MaterialGrid item xs={12} sm={6}>
          <MultilineTextField name={'albumArtist'} label="Album Artist" value={mappedTags.albumArtist || []}
                              onChange={handleChange}/>
        </MaterialGrid>
      </MaterialGrid>
      <MaterialGrid container item xs={12} spacing={1}>
        <MaterialGrid item xs={12} sm={6}>
          <TextField name={'trackNumber'} label="Track Number" value={mappedTags.trackNumber || ""}
                     onChange={handleChange}/>
        </MaterialGrid>
        <MaterialGrid item xs={12} sm={6}>
          <TextField name={'discNumber'} label="Disc Number" value={mappedTags.discNumber || ""}
                     onChange={handleChange}/>
        </MaterialGrid>
      </MaterialGrid>
      <MaterialGrid container item xs={12} spacing={1}>
        <MaterialGrid item xs={12}>
          <TextField name={'comment'} label="Comment" value={mappedTags.comment || ""} onChange={handleChange}
                     multiline rowsMax={3}/>
        </MaterialGrid>
      </MaterialGrid>
    </MaterialGrid>)
})

function MultilineTextField(props: TextFieldProps) {
  let {value, ...others} = props
  return (
    <TextField multiline value={(value as [string] || []).join('\n')} {...others}/>
  )
}

function AdvancedEditor(props: EditorProps<[API.SourceItem]>) {
  let classes = useStyles();
  const [tags, setTags] = useState(props.data);

  const handleChange = (event: React.ChangeEvent<HTMLTextAreaElement>) => {
    event.persist()
    setTags(produce(tags => {
      if (event?.target) {
        // tags[event.target.name] = event.target.value;
      }
    }))
  };

  const handleReset = () => {
    setTags(_ => props.data)
    console.log("reset form")
  }

  const handleSubmit = (event: FormEvent) => {
    event.preventDefault()
    if (props.onSuccess) {
      props.onSuccess()
    }
    console.log("submitted form")
  }

  const handleCancel = () => {
    if (props.onFailure) {
      props.onFailure()
    }
    console.log("cancelled form")
  }
  const [columns] = useState([
    { name: 'key', title: 'Key' },
    { name: 'value', title: 'Value' },
  ]);
  const sourceItem = props.data[0];
  const [rows, setRows] = useState<MetadataPair[]>(sourceItem.metadata.tags);
  const [editingCells, setEditingCells] = useState<EditingCell[]>([]);

  const commitChanges = (changes: ChangeSet) => {
    let changedRows: MetadataPair[] | undefined;
    if (changes.added) {
      changedRows = [
        ...rows,
        ...changes.added,
      ];
      setEditingCells([{ rowId: rows.length, columnName: columns[0].name }]);
    }
    if (changes.changed) {
      changedRows = rows.map((row, index) => {
        if (changes.changed && changes.changed[index]) {
          return { ...row, ...changes.changed[index] }
        }
        return row
      })
    }
    if (changes.deleted) {
      const deletedSet = new Set(changes.deleted);
      changedRows = rows.filter((_, index) => !deletedSet.has(index));
    }

    if (changedRows) {
      setRows(changedRows);
    }
  };

  const addEmptyRow = () => commitChanges({ added: [{}] });

  return (<>
      <Grid
        rows={rows}
        columns={columns}
      >
        <EditingState
          onCommitChanges={commitChanges}
          editingCells={editingCells}
          onEditingCellsChange={setEditingCells}
          addedRows={[]}
          onAddedRowsChange={addEmptyRow}
        />
        <Table  />
        <TableHeaderRow />
        <TableInlineCellEditing selectTextOnEditStart />
        <TableEditColumn
          showAddCommand
          showDeleteCommand
          commandComponent={CommandButton}
        />
      </Grid>
    </>
  );
}

const CommandButton = (props: TableEditColumn.CommandProps) => {
  const {text, onExecute} = props
  const CommandIconButton = (props: { children: React.ReactNode }) => (
    <IconButton aria-label={text} onClick={onExecute}>
      {props.children}
    </IconButton>
  )
  return (<>
    {props.id == "delete" &&
    <CommandIconButton>
        <DeleteIcon/>
    </CommandIconButton>
    }
    {props.id == "add" &&
    <CommandIconButton>
        <AddIcon/>
    </CommandIconButton>
    }
  </>)
}

function tabProps(index: any) {
  return {
    id: `simple-tab-${index}`,
    'aria-controls': `simple-tabpanel-${index}`,
  };
}

const GET_SOURCES = gql`
    query GetSources($in: [String!]!) {
        library {
            sources(where: {id: {inputname: InExpr, InExpr: {in: $in}}}) {
                id
                sourceName
                sourceUri
                format
                metadata {
                    tags {
                        key
                        value
                    }
                    mappedTags {
                        albumArtist
                        artistName
                        albumTitle
                        trackTitle
                        trackNumber
                        totalTracks
                        date
                        genre
                        discNumber
                        totalDiscs
                        comment
                        musicbrainzArtistId
                        musicbrainzAlbumArtistId
                        musicbrainzAlbumId
                        musicbrainzTrackId
                    }
                    formatId
                    format
                }
                downloadUri
            }
        }
    }
`;

export interface SourceMetadataEditorProps {
  srcIds: string[],
  onSuccess: () => void,
  onFailure: () => void,
}

export default function SourceMetadataEditor(props: SourceMetadataEditorProps) {
  let classes = useStyles();
  const [value, setValue] = React.useState(0);
  const {loading, error, data, refetch} = useQuery<API.Data>(GET_SOURCES, {
    variables: {
      in: props.srcIds
    },
    fetchPolicy: "no-cache"
  });

  const srcs = data?.library?.sources;

  return (
    <div className={classes.root}>
      {loading && (
        <LinearProgress/>
      )}
      {error && (
        <Typography>Error retrieving source metadata: {error.message}</Typography>
      )}
      {srcs && (
        <>
          <Tabs
            value={value}
            onChange={(_, index) => setValue(index)}
            indicatorColor="primary"
            textColor="primary"
            variant="fullWidth"
            aria-label="metadata editor tabs"
            className={classes.tabbar}
          >
            <Tab label="Basic" {...tabProps(0)} />
            <Tab label="Advanced" {...tabProps(1)} />
          </Tabs>

          <>
            {value == 0 && (
              <BasicEditor data={srcs} onFailure={props.onFailure} onSuccess={props.onSuccess}/>
            )}
            {value == 1 && (
              <AdvancedEditor data={srcs}
                              onFailure={props.onFailure} onSuccess={props.onSuccess}/>
            )}
          </>
          {/*{srcs?.length > 1 && (*/}
          {/*  <>*/}
          {/*    {value == 0 && (*/}
          {/*      <MultiSrcBasicEditor sources={srcs}*/}
          {/*                           onFailure={props.onFailure} onSuccess={props.onSuccess}/>*/}
          {/*    )}*/}
          {/*  </>*/}
          {/*)}*/}
        </>
      )}
    </div>
  )
}
