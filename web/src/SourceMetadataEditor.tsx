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

import 'ag-grid-community/dist/styles/ag-grid.css';
import 'ag-grid-community/dist/styles/ag-theme-material.css';
import 'ag-grid-community/dist/styles/ag-grid.css';
import LinearProgress from "@material-ui/core/LinearProgress";
import produce from "immer";
import Grid from "@material-ui/core/Grid";
import Accordian from "@material-ui/core/Accordion";
import AccordionActions from "@material-ui/core/AccordionActions";
import AccordionDetails from "@material-ui/core/AccordionDetails";
import AccordionSummary from "@material-ui/core/AccordionSummary";
import ExpandMoreIcon from "@material-ui/icons/ExpandMore";

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
    forms.forEach(([ref, a, b]) => {
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
    <Grid
      container
      spacing={1}
      justify="flex-start"
      alignItems="stretch"
    >
      <Grid container item xs={12} spacing={1}>
        <Grid item xs={12} sm={6}>
          <MultilineTextField name={'artistName'} label="Artist Name" value={mappedTags.artistName || []}
                              onChange={handleChange}/>
        </Grid>
        <Grid item xs={12} sm={6}>
          <TextField name={'trackTitle'} label="Track Title" value={mappedTags.trackTitle || ""}
                     onChange={handleChange}/>
        </Grid>
      </Grid>
      <Grid container item xs={12} spacing={1}>
        <Grid item xs={12} sm={6}>
          <TextField name={'albumTitle'} label="Album Title" value={mappedTags.albumTitle || ""}
                     onChange={handleChange}/>
        </Grid>
        <Grid item xs={12} sm={6}>
          <TextField name={'date'} label="Date" value={mappedTags.date || ""} onChange={handleChange}/>
        </Grid>
      </Grid>
      <Grid container item xs={12} spacing={1}>
        <Grid item xs={12} sm={6}>
          <MultilineTextField name={'genre'} label="Genre" value={mappedTags.genre || []} onChange={handleChange}/>
        </Grid>
        <Grid item xs={12} sm={6}>
          <MultilineTextField name={'albumArtist'} label="Album Artist" value={mappedTags.albumArtist || []}
                              onChange={handleChange}/>
        </Grid>
      </Grid>
      <Grid container item xs={12} spacing={1}>
        <Grid item xs={12} sm={6}>
          <TextField name={'trackNumber'} label="Track Number" value={mappedTags.trackNumber || ""}
                     onChange={handleChange}/>
        </Grid>
      </Grid>
      <Grid container item xs={12} spacing={1}>
        <Grid item xs={12}>
          <TextField name={'comment'} label="Comment" value={mappedTags.comment || ""} onChange={handleChange}
                     multiline rowsMax={3}/>
        </Grid>
      </Grid>
    </Grid>)
})

function MultilineTextField(props: TextFieldProps) {
  let {value, ...others} = props
  return (
    <TextField multiline value={(value as [string] || []).join('\n')} {...others}/>
  )
}

function AdvancedEditor(props: EditorProps<[API.MetadataPair]>) {
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

  return (
    <div
      role="tabpanel"
      // hidden={value !== index}
      id="single-advanced-editor"
      aria-labelledby="single-advanced-editor"
    >
        <form id="metadata-form" onSubmit={handleSubmit} onReset={handleReset} noValidate autoComplete="off">

          {/*<div className="ag-theme-material" style={ {height: '600px', width: '600px'} }>*/}
          {/*  <AgGridReact*/}
          {/*    columnDefs={columnDefs}*/}
          {/*    rowData={rows}>*/}
          {/*  </AgGridReact>*/}
          {/*</div>*/}
        </form>
    </div>
  );
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
            {/*{value == 1 && (*/}
            {/*  <AdvancedEditor tags={srcs[0]?.metadata?.tags}*/}
            {/*                  onFailure={props.onFailure} onSuccess={props.onSuccess}/>*/}
            {/*)}*/}
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
