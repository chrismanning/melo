import React, {FormEvent, useState} from 'react';
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
import {MappedTags, MappedTagsInput, MetadataPair} from "./API";
import TextField, {TextFieldProps} from "@material-ui/core/TextField";

import 'ag-grid-community/dist/styles/ag-grid.css';
import 'ag-grid-community/dist/styles/ag-theme-material.css';
import 'ag-grid-community/dist/styles/ag-grid.css';
import AppBar from "@material-ui/core/AppBar";
import Fade from "@material-ui/core/Fade";
import LinearProgress from "@material-ui/core/LinearProgress";
import produce from "immer";
import Grid from "@material-ui/core/Grid";

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

interface EditorPanelProps {
  index: number,
  value: number,
  onSuccess: () => void,
  onFailure: () => void,
}

const SAVE_MAPPED_TAGS = gql`
    mutation UpdateSourcesByMappedTags($id: String!, $mappedTags: MappedTagsInput!) {
        library {
            updateSources(updates: [{
                id: $id,
                updateTags: {
                    setMappedTags: $mappedTags
                }
            }]) {
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

function BasicEditor(props: EditorPanelProps
    & { mappedTags: MappedTags, srcId: string }) {
  let classes = useStyles();
  const [mappedTags, setMappedTags] = useState(props.mappedTags as MappedTagsInput);
  const [saveMappedTags, { error, data, loading }] = useMutation(SAVE_MAPPED_TAGS)

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.persist()
    setMappedTags(produce(mappedTags => {
      if (event?.target) {
        if (event.target.name == 'artistName'
          || event.target.name == 'albumArtist'
          || event.target.name == 'genre'
          || event.target.name == 'musicbrainzArtistId'
          || event.target.name == 'musicbrainzAlbumArtistId') {
          console.log('event.target.value: ' + JSON.stringify(event.target.value))
          mappedTags[event.target.name] = event.target.value.split('\n')
        } else {
          mappedTags[event.target.name] = event.target.value;
        }
      }
    }))
  };

  const handleReset = () => {
    setMappedTags(_ => props.mappedTags as MappedTagsInput)
    console.log("reset form")
  }

  const handleSubmit = (event: FormEvent) => {
    event.preventDefault()
    console.log("submitting form")
    saveMappedTags( {
      variables: {
        id: props.srcId,
        mappedTags: mappedTags
      }
    }).then(_ => {
      console.log("mapped tags saved")
      if (props.onSuccess) {
        props.onSuccess()
      }
    }, () => {
      if (error) {
        console.log("failed to save mapped tags: " + error.message)
        console.log("mappedTags: " + JSON.stringify(mappedTags))
        console.log("errors: " + JSON.stringify(error?.graphQLErrors))
      }
      if (props.onFailure) {
        props.onFailure()
      }
    })
  }

  const {value, index} = props;
  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`simple-tabpanel-${index}`}
      aria-labelledby={`simple-tab-${index}`}
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
      {value === index && (
        <form id="metadata-form" onSubmit={handleSubmit} onReset={handleReset} noValidate autoComplete="off">
          <Grid
            container
            spacing={1}
            // direction="column"
            justify="flex-start"
            alignItems="stretch"
          >
            <Grid container item xs={12} spacing={1}>
              <Grid item xs={12} sm={6}>
                <MultilineTextField name={'artistName'} label="Artist Name" value={mappedTags.artistName || []}
                           onChange={handleChange} fullWidth/>
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField name={'trackTitle'} label="Track Title" value={mappedTags.trackTitle || ""}
                           onChange={handleChange}/>
              </Grid>
            </Grid>
            <Grid container item xs={12} spacing={1}>
              <Grid item xs={12} sm={6}>
                <TextField name={'albumTitle'} label="Album Title" value={mappedTags.albumTitle || ""}
                           onChange={handleChange} fullWidth/>
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField name={'date'} label="Date" value={mappedTags.date || ""} onChange={handleChange}
                           fullWidth/>
              </Grid>
            </Grid>
            <Grid container item xs={12} spacing={1}>
              <Grid item xs={12} sm={6}>
                <MultilineTextField name={'genre'} label="Genre" value={mappedTags.genre || []} onChange={handleChange}/>
              </Grid>
              <Grid item xs={12} sm={6}>
                <MultilineTextField name={'albumArtist'} label="Album Artist" value={mappedTags.albumArtist || []}
                           onChange={handleChange} fullWidth/>
              </Grid>
            </Grid>
            <Grid container item xs={12} spacing={1}>
              <Grid item xs={12} sm={6}>
                <TextField name={'trackNumber'} label="Track Number" value={mappedTags.trackNumber || ""}
                           onChange={handleChange} fullWidth/>
              </Grid>
            </Grid>
            <Grid container item xs={12} spacing={1} className={classes.doubleWidth}>
              <Grid item xs={12}>
                <TextField name={'comment'} label="Comment" value={mappedTags.comment || ""} onChange={handleChange}
                           multiline rowsMax={3} fullWidth/>
              </Grid>
            </Grid>
          </Grid>
        </form>
      )}
    </div>
  );
}

function MultilineTextField(props: TextFieldProps) {
  let {value, ...others} = props
  return (
    <TextField multiline value={(value as [string] || []).join('\n')} {...others}/>
  )
}

function AdvancedEditor(props: EditorPanelProps & { tags: [MetadataPair] }) {
  let classes = useStyles();
  const [tags, setTags] = useState(props.tags);

  const handleChange = (event: React.ChangeEvent<HTMLTextAreaElement>) => {
    event.persist()
    setTags(produce(tags => {
      if (event?.target) {
        // tags[event.target.name] = event.target.value;
      }
    }))
  };

  const handleReset = () => {
    setTags(_ => props.tags)
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

  const {value, index} = props;
  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`simple-tabpanel-${index}`}
      aria-labelledby={`simple-tab-${index}`}
    >
      {value === index && (
        <form id="metadata-form" onSubmit={handleSubmit} onReset={handleReset} noValidate autoComplete="off">

          {/*<div className="ag-theme-material" style={ {height: '600px', width: '600px'} }>*/}
          {/*  <AgGridReact*/}
          {/*    columnDefs={columnDefs}*/}
          {/*    rowData={rows}>*/}
          {/*  </AgGridReact>*/}
          {/*</div>*/}
        </form>
      )}
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
  srcId: String,
  onSuccess: () => void,
  onFailure: () => void,
}

export default function SourceMetadataEditor(props: SourceMetadataEditorProps) {
  let classes = useStyles();
  const [value, setValue] = React.useState(0);
  const {loading, error, data, refetch} = useQuery<API.Data>(GET_SOURCES, {
    variables: {
      in: [props.srcId]
    },
    fetchPolicy: "no-cache"
  });

  const srcs = data?.library?.sources;

  return (
    <div className={classes.root}>
      <div>

        <Fade
          in={loading}
          style={{
            transitionDelay: loading ? '800ms' : '0ms',
          }}
          unmountOnExit
        >
          <LinearProgress/>
        </Fade>
      </div>
      {error && (
        <Typography>Error retrieving source metadata: {error.message}</Typography>
      )}
      {srcs && (
        <>
          <AppBar position="static" color="default" className={classes.tabbar}>
            <Tabs
              value={value}
              onChange={(_, index) => setValue(index)}
              indicatorColor="primary"
              textColor="primary"
              variant="fullWidth"
              aria-label="metadata editor tabs"
            >
              <Tab label="Basic" {...tabProps(0)} />
              <Tab label="Advanced" {...tabProps(1)} />
            </Tabs>
          </AppBar>

          <BasicEditor index={0} value={value} mappedTags={srcs[0]?.metadata?.mappedTags} srcId={srcs[0].id}
                       onFailure={props.onFailure} onSuccess={props.onSuccess}/>
          <AdvancedEditor index={1} value={value} tags={srcs[0]?.metadata?.tags}
                          onFailure={props.onFailure} onSuccess={props.onSuccess}/>
        </>
      )}
    </div>
  )
}
