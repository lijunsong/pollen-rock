import React, { Component } from 'react';
import Path from 'path';
import * as Icons from './Icons';
import { Map, Set, List } from 'immutable';
import * as Api from './Api';
import PropTypes from 'prop-types';
import * as Notify from './Notify';


function NavHeader(props) {
  return <div id="NavHeader">Entries</div>;
}


/// NavEntries is responsible for the view of files and folders, and
/// the events on clicking the entry
class NavEntries extends Component {
  constructor(props) {
    super(props);
    this.state = {
      // a map from folder name to children names.  The key is path,
      // value is it's children, a list of string.  Same as
      // pollen-rock fs api, if an item ends with '/', the item is a
      // folder
      entries: Map({"/": List(["Loading"])}),
      // a set of paths that have expanded
      expanded: Set(),
      // selected entry. There will only be one entry selected
      selectedPath: null,
    };
  }

  onClickFile(path) {
    if (this.props.onClickFile) {
      this.props.onClickFile(path);
    }
    this.setState({selectedPath: path});
  }

  onClickFolder(path) {
    if (this.state.expanded.has(path)) {
      let expanded = this.state.expanded.delete(path);
      this.setState({expanded});
      return;
    }

    this.fetchFileList(path).then(() => {
      let expanded = this.state.expanded.add(path);
      this.setState({expanded});
    });
  }

  /// recursively render folders and it's children
  renderFolder(parentPath, folderName) {
    const fullPath = Path.join(parentPath, folderName);

    let arrow = <Icons.IconArrowRight className="textSizedIcons"/>;
    let childrenView = "";

    // render all children if this folder is expanded
    if (this.state.expanded.has(fullPath)) {
      // children must not be null (because expanded is
      // a subset of entries)
      const children = this.state.entries.get(fullPath);
      childrenView = (
        <div className="entrySpacer">
          {this.renderEntryList(fullPath, children)}
        </div>
      );

      arrow = <Icons.IconArrowDown className="textSizedIcons" />;
    }

    const folder = (
      <span className="entry"
            onClick={this.onClickFolder.bind(this, fullPath)}>
        {arrow}{folderName}
      </span>
    );

    return <div className="isDir" key={fullPath}>
             {folder}
             {childrenView}
           </div>;
  }

  /// render a single file entry
  renderFile(parentPath, fileName) {
    let clsNames = ["isFile", "entry"];
    const fullPath = Path.join(parentPath, fileName);
    if (this.state.selectedPath === fullPath) {
      clsNames.push("selectedEntry");
    }

    if (fullPath.endsWith(".html")) {
      clsNames.push("pollenDestFile");
    }

    let className = clsNames.join(" ");
    return <div className={className} key={fullPath}
                onClick={this.onClickFile.bind(this, fullPath)}>
             {fileName}
           </div>;
  }

  /// Return a list of rendered entries
  renderEntryList(parentPath, list) {
    return list.map((e) => {
      if (e.endsWith("/")) {
        return this.renderFolder(parentPath, e);
      } else {
        return this.renderFile(parentPath, e);
      }
    });
  }

  render() {
    const parentPath = "/";
    const list = this.state.entries.get(parentPath);
    let renderedList = this.renderEntryList(parentPath, list);

    if (renderedList.isEmpty()) {
      renderedList = "No files";
    }
    return <div id="NavEntries">{renderedList}</div>;
  }

  async fetchFileList(folderPath) {
    let res;
    try {
      res = await Api.getContents(folderPath);
    } catch (err) {
      let msg = `Couldn't fetch files under ${folderPath}: ${err}`;
      Notify.error(msg);
      return;
    }
    let errno = res.data.errno;
    if (errno !== 0) {
      let msg = `Error occurred on folder ${folderPath}: errno = ${errno}`;
      Notify.error(msg);
      return;
    }
    if ('items' in res.data) {
      console.log(`set ${folderPath} in the entries map`);
      let list = List(res.data.items);
      const folders = list.filter(f => f.endsWith("/"));
      const files = list.filterNot(f => f.endsWith("/"));
      list = folders.concat(files);
      let entries = this.state.entries.set(folderPath, list);
      this.setState({entries});
    } else {
      Notify.error(`${folderPath} is not a folder`);
      return;
    }
  }

  componentDidMount() {
    this.fetchFileList("/");
  }
}


/// See NavEntries for required props
class Navigation extends Component {
  render() {
    return (
      <div id="Navigation">
        <NavHeader />
        <NavEntries
          onClickFile={this.props.onClickFile}
        />
      </div>
    );
  }
}

Navigation.propTypes = {
  onClickFile: PropTypes.func.isRequired,
};

export default Navigation;
