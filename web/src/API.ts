export interface Data {
  library?: Library
}

export interface Library {
  sources?: [SourceItem],
  sourceGroups?: [SourceGroup],
  collections?: [Collection],
}

export interface Collection {
  id: string,
  name: string,
  sourceGroups?: [SourceGroup],
  sources?: [SourceItem],
}

export interface SourceGroup {
  groupParentUri?: string,
  groupTags: GroupTags,
  sources: [SourceItem],
}

export interface SourceItem {
  id: string,
  format: string,
  metadata: Metadata,
  sourceName: string,
  sourceUri: string,
  parentUri: string,
  downloadUri: string,
  length: number
}

export interface EditableSourceItem {
  id: string,
  format: string,
  metadata: EditableMetadata,
  sourceName: string,
  sourceUri: string,
  parentUri: string,
  downloadUri: string,
  length: number
}

export interface EditableMetadata {
  tags: [MetadataPair],
  mappedTags: {[values: string]: any},
  formatId: string,
  format: string
}

export interface Metadata {
  tags: [MetadataPair],
  mappedTags: MappedTags,
  formatId: string,
  format: string
}

export interface MetadataPair {
  key: string,
  value: string
}

export interface MappedTags {
  artistName?: [string],
  trackTitle?: string,
  albumTitle?: string,
  date?: string,
  genre?: [string],
  albumArtist?: [string],
  trackNumber?: string,
  totalTracks?: string,
  discNumber?: string,
  totalDiscs?: string,
  comment?: string,
  musicbrainzArtistId?: [string],
  musicbrainzAlbumArtistId?: [string],
  musicbrainzAlbumId?: string,
  musicbrainzTrackId?: string,
}

export interface MappedTagsInput {
  artistName?: [string],
  trackTitle?: string,
  albumTitle?: string,
  date?: string,
  genre?: [string],
  albumArtist?: [string],
  trackNumber?: string,
  totalTracks?: string,
  discNumber?: string,
  totalDiscs?: string,
  comment?: string,
  musicbrainzArtistId?: [string],
  musicbrainzAlbumArtistId?: [string],
  musicbrainzAlbumId?: string,
  musicbrainzTrackId?: string,
}

export interface GroupTags {
  albumArtist?: [string],
  albumTitle?: string,
  date?: string,
  genre?: [string],
  totalTracks?: string,
  discNumber?: string,
  totalDiscs?: string,
  musicbrainzArtistId?: [string],
  musicbrainzAlbumArtistId?: [string],
  musicbrainzAlbumId?: string,
}
